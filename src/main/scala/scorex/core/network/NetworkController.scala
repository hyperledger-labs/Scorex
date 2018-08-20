package scorex.core.network

import java.net._

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.message.{Message, MessageHandler, MessageSpec}
import scorex.core.network.peer.{LocalAddressPeerFeature, LocalAddressPeerFeatureSerializer, PeerInfo}
import scorex.core.serialization.Serializer
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging, TimeProvider}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(settings: NetworkSettings,
                        messageHandler: MessageHandler,
                        features: Seq[PeerFeature],
                        upnp: Option[UPnPGateway],
                        peerManagerRef: ActorRef,
                        timeProvider: TimeProvider,
                        tcpManager: ActorRef
                       )(implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  import NetworkController.ReceivableMessages._
  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
  import PeerConnectionHandler.ReceivableMessages.CloseConnection
  import scorex.core.network.peer.PeerManager.ReceivableMessages.{CheckPeers, Disconnected, FilterPeers}

  private implicit val system: ActorSystem = context.system

  private val localPeerFeatureIdAndSerializer  = LocalAddressPeerFeature.featureId -> LocalAddressPeerFeatureSerializer
  private val featureSerializers: PeerFeature.Serializers = (
    features.map(f => f.featureId -> (f.serializer:Serializer[_ <: PeerFeature])) :+ localPeerFeatureIdAndSerializer
  ).toMap

  private val handshakeSerializer = new HandshakeSerializer(featureSerializers, settings.maxHandshakeSize)

  //todo: make usage more clear, now we're relying on preStart logic in a actor which is described by a never used val
  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManagerRef, settings)

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val messageHandlers = mutable.Map[Message.MessageCode, ActorRef]()

  //check own declared address for validity
  if (!settings.localOnly) {
    settings.declaredAddress.foreach { myAddress =>
      Try {
        val uri = new URI("http://" + myAddress)
        val myHost = uri.getHost
        val myAddrs = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.asScala.exists { intf =>
          intf.getInterfaceAddresses.asScala.exists { intfAddr =>
            val extAddr = intfAddr.getAddress
            myAddrs.contains(extAddr)
          }
        } || (upnp.exists(u => myAddrs.exists(_ == u.externalAddress)))
      } recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
      }
    }
  }

  lazy val bindAddress = settings.bindAddress

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.declaredAddress orElse {
      upnp.map(u => new InetSocketAddress(u.externalAddress, settings.bindAddress.getPort))
    } orElse {
      getPublicAddress
    }
  }

  log.info(s"Declared address: $externalSocketAddress")

  lazy val connTimeout = Some(settings.connectionTimeout)

  //bind to listen incoming connections
  tcpManager ! Bind(self, bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + settings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManagerRef ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.bindAddress.getPort + " already in use!")
      context stop self
    //TODO catch?
  }

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.get(msgId) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error(s"No handlers found for message $remote: " + msgId)
            //todo: ban a peer
          }
        case Failure(e) =>
          log.error(s"Failed to deserialize data from $remote: ", e)
        //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      (peerManagerRef ? FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  private val outgoing = mutable.Set[InetSocketAddress]()

  def peerLogic: Receive = {
    case ConnectTo(peer) =>
      log.info(s"Connecting to peer: $peer")
      val remote = getPeerAddress(peer)

      outgoing += remote
      tcpManager ! Connect(remote,
                          options = KeepAlive(true) :: Nil,
                          timeout = connTimeout,
                          pullMode = true) //todo: check pullMode flag

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.handlerRef ! CloseConnection
      peerManagerRef ! Disconnected(peer.socketAddress)

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
      // todo: the following message might become unnecessary if we refactor PeerManager to automatically
      // todo: remove peer from `connectedPeers` on receiving `AddToBlackList` message.
      peerManagerRef ! Disconnected(peer.socketAddress)

    case Connected(remote, local) =>
      val direction: ConnectionType = if(outgoing.contains(remote)) Outgoing else Incoming
      val logMsg = direction match {
        case Incoming => s"New incoming connection from $remote established (bound to local $local)"
        case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
      }
      log.info(logMsg)
      val connection = sender()

      val peerFeatures = if (remote.getAddress.isSiteLocalAddress() || remote.getAddress.isLoopbackAddress()) {
        features :+ LocalAddressPeerFeature(new InetSocketAddress(local.getAddress, settings.bindAddress.getPort))
      } else {
        features
      }

      val connectionDescription = ConnectionDescription(
        connection, direction, externalSocketAddress, remote, peerFeatures)

      val handlerProps: Props = PeerConnectionHandlerRef.props(settings, self, peerManagerRef,
        messageHandler, handshakeSerializer, connectionDescription, timeProvider)
      context.actorOf(handlerProps) // launch connection handler
      outgoing -= remote

    case f@CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      f.cause match {
        case Some(t) =>
          log.info("Failed to connect to : " + c.remoteAddress, t)
        case None =>
          log.info("Failed to connect to : " + c.remoteAddress)
      }
      peerManagerRef ! Disconnected(c.remoteAddress)
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      (peerManagerRef ? FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! CloseConnection))
      self ! Unbind
      context stop self
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers ++= specs.map(_.messageCode -> handler)

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  private def getPublicAddress = {
    if (bindAddress.getAddress.isAnyLocalAddress) {
      val allAddrs = NetworkInterface.getNetworkInterfaces.asScala
        .flatMap(_.getInetAddresses.asScala)
        .collect { case a: Inet4Address => a}
        .toList

      val addr = allAddrs.filterNot(a => a.isSiteLocalAddress || a.isLoopbackAddress).headOption
      addr.map(a => new InetSocketAddress(a, bindAddress.getPort))
    } else if (!bindAddress.getAddress.isSiteLocalAddress) {
      Some(bindAddress)
    } else {
      None
    }
  }

  private def getPeerAddress(peer: PeerInfo): InetSocketAddress = {
    val peerLocalAddress = peer.features.collectFirst { case LocalAddressPeerFeature(addr) => addr }
    val remote = peerLocalAddress match {
      case Some(addr) =>
        addr

      case None if externalSocketAddress.exists(_.getAddress == peer.decalerdAddress.getAddress) =>
        val addr = upnp.flatMap(_.getLocalAddressForExternalPort(peer.decalerdAddress.getPort))
        addr.getOrElse(peer.decalerdAddress)

      case _ => peer.decalerdAddress
    }
    remote
  }
}

object NetworkController {
  object ReceivableMessages {
    case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)
    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)
    case object ShutdownNetwork
    case class ConnectTo(peer: PeerInfo)
    case class DisconnectFrom(peer: ConnectedPeer)
    case class Blacklist(peer: ConnectedPeer)
  }
}

object NetworkControllerRef {
  def props(settings: NetworkSettings,
            messageHandler: MessageHandler,
            features: Seq[PeerFeature],
            upnp: Option[UPnPGateway],
            peerManagerRef: ActorRef,
            timeProvider: TimeProvider,
            tcpManager: ActorRef)(implicit ec: ExecutionContext): Props =
    Props(new NetworkController(settings, messageHandler, features, upnp, peerManagerRef, timeProvider, tcpManager))

  def apply(settings: NetworkSettings,
            messageHandler: MessageHandler,
            features: Seq[PeerFeature],
            upnp: Option[UPnPGateway],
            peerManagerRef: ActorRef,
            timeProvider: TimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, messageHandler, features, upnp, peerManagerRef, timeProvider, IO(Tcp)))

  def apply(name: String,
            settings: NetworkSettings,
            messageHandler: MessageHandler,
            features: Seq[PeerFeature],
            upnp: Option[UPnPGateway],
            peerManagerRef: ActorRef,
            timeProvider: TimeProvider,
            tcpManager: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, messageHandler, features, upnp, peerManagerRef, timeProvider, tcpManager), name)

  def apply(name: String,
            settings: NetworkSettings,
            messageHandler: MessageHandler,
            features: Seq[PeerFeature],
            upnp: Option[UPnPGateway],
            peerManagerRef: ActorRef,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, messageHandler, features, upnp, peerManagerRef, timeProvider, IO(Tcp)), name)
}