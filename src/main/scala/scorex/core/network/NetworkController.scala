package scorex.core.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.message.{Message, MessageHandler, MessageSpec}
import scorex.core.network.peer.PeerManager
import scorex.core.network.peer.PeerManager.EventType
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.existentials
import scala.reflect.runtime.universe.TypeTag
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(settings: NetworkSettings,
                        messageHandler: MessageHandler,
                        upnp: UPnP,
                        peerManagerRef: ActorRef,
                        timeProvider: NetworkTimeProvider
                       ) extends Actor with ScorexLogging {

  import NetworkController._

  private val synchronizerProps = Props(new PeerSynchronizer(self, peerManagerRef, settings))
  private val peerSynchronizer = context.system.actorOf(synchronizerProps, "PeerSynchronizer")

  private implicit val system: ActorSystem = context.system

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  private val tcpManager = IO(Tcp)

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
        } match {
          case true => true
          case false =>
            if (settings.upnpEnabled) {
              val extAddr = upnp.externalAddress
              myAddrs.contains(extAddr)
            } else false
        }
      }.recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
      }
    }
  }

  lazy val localAddress = settings.bindAddress

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.declaredAddress orElse {
      if (settings.upnpEnabled) {
        upnp.externalAddress.map(a => new InetSocketAddress(a, settings.bindAddress.getPort))
      } else None
    }
  }

  log.info(s"Declared address: $externalSocketAddress")


  lazy val connTimeout = Some(settings.connectionTimeout)

  //bind to listen incoming connections
  tcpManager ! Bind(self, localAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + settings.bindAddress.getPort)
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManagerRef ! PeerManager.CheckPeers)

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
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error("No handlers found for message: " + msgId)
            //todo: ban a peer
          }
        case Failure(e) =>
          log.error("Failed to deserialize data: ", e)
        //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      (peerManagerRef ? PeerManager.FilterPeers(sendingStrategy))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! message))
  }

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      tcpManager ! Connect(remote,
                          localAddress = externalSocketAddress,
                          options = KeepAlive(true) :: Nil,
                          timeout = connTimeout,
                          pullMode = true)

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.socketAddress}")
      peer.handlerRef ! PeerConnectionHandler.CloseConnection
      peerManagerRef ! PeerManager.Disconnected(peer.socketAddress)

    case Blacklist(peer) =>
      peer.handlerRef ! PeerConnectionHandler.Blacklist
      // todo: the following message might become unnecessary if we refactor PeerManager to automatically
      // todo: remove peer from `connectedPeers` on receiving `AddToBlackList` message.
      peerManagerRef ! PeerManager.Disconnected(peer.socketAddress)

    case Connected(remote, local) =>
      log.info(s"New connection from $remote to $local")
      val connection = sender()
      val handlerProps = Props(new PeerConnectionHandler(settings, self, peerManagerRef,
        messageHandler, connection, externalSocketAddress, remote, timeProvider))
      context.actorOf(handlerProps) // launch connection handler

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      peerManagerRef ! PeerManager.Disconnected(c.remoteAddress)
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      (peerManagerRef ? PeerManager.FilterPeers(Broadcast))
        .map(_.asInstanceOf[Seq[ConnectedPeer]])
        .foreach(_.foreach(_.handlerRef ! PeerConnectionHandler.CloseConnection))
      self ! Unbind
      context stop self

    case SubscribePeerManagerEvent(events) =>
      peerManagerRef ! PeerManager.Subscribe(sender(), events)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {

  case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

  case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

  case object ShutdownNetwork

  case class ConnectTo(address: InetSocketAddress)

  case class DisconnectFrom(peer: ConnectedPeer)

  case class Blacklist(peer: ConnectedPeer)

  case class DataFromPeer[DT: TypeTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)

  case class SubscribePeerManagerEvent(events: Seq[EventType.Value])
}