package scorex.core.network

import java.net._

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.app.{ScorexContext, Version}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.{Message, MessageSpec}
import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeerExcluding, RemovePeer}
import scorex.core.network.peer.{LocalAddressPeerFeature, PeerInfo}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkUtils
import scorex.util.ScorexLogging

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
                        peerManagerRef: ActorRef,
                        scorexContext: ScorexContext,
                        tcpManager: ActorRef
                       )(implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  import NetworkController.ReceivableMessages._
  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
  import PeerConnectionHandler.ReceivableMessages.CloseConnection

  private implicit val system: ActorSystem = context.system

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val messageHandlers = mutable.Map[MessageCode, ActorRef]()

  private lazy val bindAddress = settings.bindAddress

  private var connections = Map.empty[InetSocketAddress, ConnectedPeer]
  private var outgoing = Set.empty[InetSocketAddress]

  //todo: make usage more clear, now we're relying on preStart logic in a actor which is described by a never used val
  private val featureSerializers: PeerFeature.Serializers = scorexContext.features.map(f => f.featureId -> f.serializer).toMap
  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManagerRef, settings,
    featureSerializers)

  //check own declared address for validity
  validateDeclaredAddress()

  log.info(s"Declared address: ${scorexContext.externalNodeAddress}")

  //bind to listen incoming connections
  tcpManager ! Bind(self, bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + settings.bindAddress.getPort)
      scheduleConnectToPeer()

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.bindAddress.getPort + " already in use!")
      context stop self
    //TODO catch?
  }

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytesTry(msgBytes) match {
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
      filterConnections(sendingStrategy, message.spec.protocolVersion).foreach { connectedPeer =>
        connectedPeer.handlerRef ! message
      }
  }

  def peerLogic: Receive = {
    case ConnectTo(peer) =>
      connectTo(peer)

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.remote}")
      peer.handlerRef ! CloseConnection

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.ReceivableMessages.Blacklist
    // todo: the following message might become unnecessary if we refactor PeerManager to automatically
    // todo: remove peer from `connectedPeers` on receiving `AddToBlackList` message.

    case Connected(remote, local) =>
      val connection = sender()
      if (connectionForPeerAddress(remote).isEmpty) {
        createPeerConnectionHandler(remote, local, connection)
      } else {
        log.warn(s"Connection to peer $remote is already established")
        connection ! Close
      }

    case Handshaked(connectedPeer) =>
      handshaked(connectedPeer, sender())

    case f@CommandFailed(c: Connect) =>
      outgoing -= c.remoteAddress
      f.cause match {
        case Some(t) =>
          log.info("Failed to connect to : " + c.remoteAddress, t)
        case None =>
          log.info("Failed to connect to : " + c.remoteAddress)
      }
      // remove not responding peer from database
      peerManagerRef ! RemovePeer(c.remoteAddress)

    case Terminated(ref) =>
      connectionForHandler(ref).foreach { connectedPeer =>
        val addr = connectedPeer.remote
        connections -= addr
        outgoing -= addr
        context.system.eventStream.publish(DisconnectedPeer(addr))
      }
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case GetConnectedPeers =>
      sender() ! connections.values.flatMap(_.peerInfo).toSeq

    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      filterConnections(Broadcast, Version.initial).foreach { connectedPeer =>
        connectedPeer.handlerRef ! CloseConnection
      }
      self ! Unbind
      context stop self
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessageSpecs(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers ++= specs.map(_.messageCode -> handler)

    case CommandFailed(cmd: Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  /**
    * Schedule a periodic connection to a random known peer
    */
  private def scheduleConnectToPeer(): Unit = {
    context.system.scheduler.schedule(5.seconds, 5.seconds) {
      if (connections.size < settings.maxConnections) {
        val randomPeerF = peerManagerRef ? RandomPeerExcluding(connections.values.flatMap(_.peerInfo).toSeq)
        randomPeerF.mapTo[Option[PeerInfo]].foreach { peerInfoOpt =>
          peerInfoOpt.foreach(peerInfo => self ! ConnectTo(peerInfo))
        }
      }
    }
  }

  /**
    * Connect to peer
    *
    * @param peer - PeerInfo
    */
  private def connectTo(peer: PeerInfo): Unit = {
    log.info(s"Connecting to peer: $peer")
    getPeerAddress(peer) match {
      case Some(remote) =>
        if (connectionForPeerAddress(remote).isEmpty && !outgoing.contains(remote)) {
          outgoing += remote
          tcpManager ! Connect(remote,
            options = KeepAlive(true) :: Nil,
            timeout = Some(settings.connectionTimeout),
            pullMode = true) //todo: check pullMode flag
        } else {
          log.warn(s"Connection to peer $remote is already established")
        }
      case None =>
        log.warn(s"Can't obtain remote address for peer $peer")
    }
  }

  /**
    * Creates a PeerConnectionHandler for the established connection
    *
    * @param remote     - remote address of socket to peer
    * @param local      - local address of socket to peer
    * @param connection - connection ActorRef
    */
  private def createPeerConnectionHandler(remote: InetSocketAddress,
                                          local: InetSocketAddress,
                                          connection: ActorRef): Unit = {
    val direction: ConnectionType = if (outgoing.contains(remote)) Outgoing else Incoming
    val logMsg = direction match {
      case Incoming => s"New incoming connection from $remote established (bound to local $local)"
      case Outgoing => s"New outgoing connection to $remote established (bound to local $local)"
    }
    log.info(logMsg)

    val peerFeatures = if (remote.getAddress.isSiteLocalAddress || remote.getAddress.isLoopbackAddress) {
      scorexContext.features :+ LocalAddressPeerFeature(new InetSocketAddress(local.getAddress, settings.bindAddress.getPort))
    } else {
      scorexContext.features
    }

    val connectionDescription = ConnectionDescription(
      connection, direction, getNodeAddressForPeer(local), remote, peerFeatures)

    val handlerProps: Props = PeerConnectionHandlerRef.props(settings, self, peerManagerRef,
      scorexContext, connectionDescription)

    val handler = context.actorOf(handlerProps) // launch connection handler
    context.watch(handler)
    val connectedPeer = ConnectedPeer(remote, handler, None)
    connections += remote -> connectedPeer
    outgoing -= remote
  }

  /**
    * The logic of handling the handshake
    *
    * @param peerInfo
    * @param peerHandler
    */
  private def handshaked(peerInfo: PeerInfo, peerHandler: ActorRef): Unit = {
    connectionForHandler(peerHandler).foreach { connectedPeer =>

      log.trace(s"Got handshake from $peerInfo")
      val peerAddress = peerInfo.peerData.address.getOrElse(connectedPeer.remote)

      //drop connection to self if occurred or peer already connected
      if (isSelf(connectedPeer.remote) || connectionForPeerAddress(peerAddress).exists(_.handlerRef != peerHandler)) {
        connectedPeer.handlerRef ! CloseConnection
        peerManagerRef ! RemovePeer(peerAddress)
        connections -= connectedPeer.remote
      } else {
        if (peerInfo.peerData.reachablePeer) {
          peerManagerRef ! AddOrUpdatePeer(peerInfo)
        } else {
          peerManagerRef ! RemovePeer(peerAddress)
        }
        val updatedConnectedPeer = connectedPeer.copy(peerInfo = Some(peerInfo))
        connections += connectedPeer.remote -> updatedConnectedPeer
        context.system.eventStream.publish(HandshakedPeer(updatedConnectedPeer))
      }
    }
  }

  /**
    * Returns connections filtered by given SendingStrategy and Version.
    * Exclude all connections with lower version and apply sendingStrategy to remaining connected peers
    *
    * @param sendingStrategy - SendingStrategy
    * @param version         - minimal version required
    * @return sequence of ConnectedPeer instances according SendingStrategy
    */
  private def filterConnections(sendingStrategy: SendingStrategy, version: Version): Seq[ConnectedPeer] = {
    sendingStrategy.choose(connections.values.toSeq.filter(_.peerInfo.exists(_.peerData.protocolVersion >= version)))
  }

  /**
    * Returns connection for given PeerConnectionHandler ActorRef
    *
    * @param handler ActorRef on PeerConnectionHandler actor
    * @return Some(ConnectedPeer) when the connection exists for this handler, and None otherwise
    */
  private def connectionForHandler(handler: ActorRef) = {
    connections.values.find { connectedPeer =>
      connectedPeer.handlerRef == handler
    }
  }

  /**
    * Returns connection for given address of the peer
    *
    * @param peerAddress - socket address of peer
    * @return Some(ConnectedPeer) when the connection exists for this peer, and None otherwise
    */
  private def connectionForPeerAddress(peerAddress: InetSocketAddress) = {
    connections.values.find { connectedPeer =>
      connectedPeer.remote == peerAddress ||
        connectedPeer.peerInfo.exists(peerInfo => getPeerAddress(peerInfo).contains(peerAddress))
    }
  }

  /**
    * Checks the node owns the address
    *
    * @param peerAddress
    * @return returns `true` if the peer is the same is this node.
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, bindAddress, scorexContext.externalNodeAddress)
  }

  /**
    * Returns local address of peer for local connections and WAN address of peer for
    * external connections. When local address is not known, try to ask it at the UPnP gateway
    *
    * @param peer - known information about peer
    * @return socket address of the peer
    */
  private def getPeerAddress(peer: PeerInfo): Option[InetSocketAddress] = {
    (peer.peerData.localAddressOpt, peer.peerData.declaredAddress) match {
      case (Some(localAddr), _) =>
        Some(localAddr)

      case (None, Some(declaredAddress))
        if scorexContext.externalNodeAddress.exists(_.getAddress == declaredAddress.getAddress) =>

        scorexContext.upnpGateway.flatMap(_.getLocalAddressForExternalPort(declaredAddress.getPort))

      case _ => peer.peerData.declaredAddress
    }
  }

  /**
    * Returns the node address reachable from Internet
    *
    * @param localSocketAddress - local socket address of the connection to the peer
    * @return - socket address of the node
    */
  private def getNodeAddressForPeer(localSocketAddress: InetSocketAddress) = {
    val localAddr = localSocketAddress.getAddress
    scorexContext.externalNodeAddress match {
      case Some(extAddr) =>
        Some(extAddr)

      case None =>
        if (!localAddr.isSiteLocalAddress && !localAddr.isLoopbackAddress
          && localSocketAddress.getPort == settings.bindAddress.getPort) {
          Some(localSocketAddress)
        } else {
          val listenAddrs = NetworkUtils.getListenAddresses(settings.bindAddress)
            .filterNot(addr => addr.getAddress.isSiteLocalAddress || addr.getAddress.isLoopbackAddress)

          listenAddrs.find(addr => localAddr == addr.getAddress).orElse(listenAddrs.headOption)
        }
    }
  }

  private def validateDeclaredAddress() = {
    if (!settings.localOnly) {
      settings.declaredAddress.foreach { myAddress =>
        Try {
          val uri = new URI("http://" + myAddress)
          val myHost = uri.getHost
          val myAddrs = InetAddress.getAllByName(myHost)

          val listenAddresses = NetworkUtils.getListenAddresses(bindAddress)
          val upnpAddress = scorexContext.upnpGateway.map(_.externalAddress)

          val valid = listenAddresses.exists(myAddrs.contains) || upnpAddress.exists(myAddrs.contains)

          if (!valid) {
            log.error(
              s"""Declared address validation failed:
                 | $myAddress not match any of the listening address: $listenAddresses
                 | or Gateway WAN address: $upnpAddress""".stripMargin)
          }
        } recover { case t: Throwable =>
          log.error("Declared address validation failed: ", t)
        }
      }
    }
  }
}

object NetworkController {

  object ReceivableMessages {

    case class Handshaked(peer: PeerInfo)

    case class RegisterMessageSpecs(specs: Seq[MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case object ShutdownNetwork

    case class ConnectTo(peer: PeerInfo)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class Blacklist(peer: ConnectedPeer)

    case object GetConnectedPeers

  }

}

object NetworkControllerRef {
  def props(settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            tcpManager: ActorRef)(implicit ec: ExecutionContext): Props = {
    Props(new NetworkController(settings, peerManagerRef, scorexContext, tcpManager))
  }

  def apply(settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, scorexContext, IO(Tcp))
    )
  }

  def apply(name: String,
            settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            tcpManager: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, scorexContext, tcpManager),
      name)
  }

  def apply(name: String,
            settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {

    system.actorOf(
      props(settings, peerManagerRef, scorexContext, IO(Tcp)),
      name)
  }
}