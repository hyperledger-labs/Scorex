package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import scorex.core.app.Version
import scorex.core.network.PeerConnectionHandler.{AwaitingHandshake, Handshaked, WorkingCycle}
import scorex.core.network.message.MessageHandler
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.AddToBlacklist
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{ScorexLogging, TimeProvider}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}


sealed trait ConnectionType
case object Incoming extends ConnectionType
case object Outgoing extends ConnectionType


case class ConnectedPeer(remote: InetSocketAddress,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  import shapeless.syntax.typeable._

  override def hashCode(): Int = remote.hashCode()

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(p => p.remote == this.remote && peerInfo == this.peerInfo)

  override def toString: String = s"ConnectedPeer($remote)"
}

case object Ack extends Event


case class ConnectionDescription(connection: ActorRef,
                                 direction: ConnectionType,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 remote: InetSocketAddress,
                                 localFeatures: Seq[PeerFeature])

class PeerConnectionHandler(val settings: NetworkSettings,
                            networkControllerRef: ActorRef,
                            peerManagerRef: ActorRef,
                            messageHandler: MessageHandler,
                            handshakeSerializer: HandshakeSerializer,
                            connectionDescription: ConnectionDescription,
                            timeProvider: TimeProvider)(implicit ec: ExecutionContext)
  extends Actor with Buffering with ScorexLogging {

  import PeerConnectionHandler.ReceivableMessages._

  private lazy val connection = connectionDescription.connection
  private lazy val direction = connectionDescription.direction
  private lazy val ownSocketAddress = connectionDescription.ownSocketAddress
  private lazy val remote = connectionDescription.remote
  private lazy val localFeatures = connectionDescription.localFeatures


  context watch connection

  // there is no recovery for broken connections
  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy


  private var receivedHandshake: Option[Handshake] = None
  private var selfPeer: Option[ConnectedPeer] = None

  //todo: use `become` to handle handshake state instead?
  private def handshakeGot = receivedHandshake.isDefined

  private var handshakeSent = false

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var chunksBuffer: ByteString = CompactByteString()

  private def handshake: Receive =
    startInteraction orElse
      receivedData orElse
      handshakeTimeout orElse
      handshakeDone orElse
      processErrors(AwaitingHandshake.toString)

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
      //      peerManager ! AddToBlacklist(remote)
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting

    case cc: ConnectionClosed =>
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context stop self

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd + s" in state $stateName")
      connection ! ResumeReading
  }

  private def startInteraction: Receive = {
    case StartInteraction =>
      val hb = handshakeSerializer.toBytes(Handshake(settings.agentName,
        Version(settings.appVersion), settings.nodeName,
        ownSocketAddress, localFeatures, timeProvider.time()))

      connection ! Tcp.Write(ByteString(hb))
      log.info(s"Handshake sent to $remote")
      handshakeSent = true
      if (handshakeGot && handshakeSent) self ! HandshakeDone
  }

  private def receivedData: Receive = {
    case Received(data) =>
      handshakeSerializer.parseBytes(data.toArray) match {
        case Success(handshake) =>
          receivedHandshake = Some(handshake)
          log.info(s"Got a Handshake from $remote")
          connection ! ResumeReading
          if (handshakeGot && handshakeSent) self ! HandshakeDone
        case Failure(t) =>
          log.info(s"Error during parsing a handshake", t)
          //todo: blacklist?
          self ! CloseConnection
      }
  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $remote, going to drop the connection")
      self ! CloseConnection
  }

  private def handshakeDone: Receive = {
    case HandshakeDone =>
      require(receivedHandshake.isDefined)

      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val handshake = receivedHandshake.get
      val peerInfo = PeerInfo(
        timeProvider.time(),
        handshake.declaredAddress,
        Some(handshake.nodeName),
        Some(direction),
        handshake.features
      )
      val peer = ConnectedPeer(remote, self, Some(peerInfo))
      selfPeer = Some(peer)

      networkControllerRef ! Handshaked(peerInfo)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycle
  }

  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
      def sendOutMessage() {
        val bytes = msg.bytes
        log.info("Send message " + msg.spec + " to " + remote)
        connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))
      }

      //simulating network delays
      settings.addedMaxDelay match {
        case Some(delay) =>
          context.system.scheduler.scheduleOnce(Random.nextInt(delay.toMillis.toInt).millis)(sendOutMessage())
        case None =>
          sendOutMessage()
      }

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
      peerManagerRef ! AddToBlacklist(remote)
      connection ! Close
  }

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2

      t._1.find { packet =>
        messageHandler.parseBytes(packet.toByteBuffer, selfPeer) match {
          case Success(message) =>
            log.info("Received message " + message.spec + " from " + remote)
            networkControllerRef ! message
            false

          case Failure(e) =>
            log.info(s"Corrupted data from: " + remote, e)
            //  connection ! Close
            //  context stop self
            true
        }
      }
      connection ! ResumeReading
  }

  private def reportStrangeInput: Receive= {
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(WorkingCycle.toString) orElse
      reportStrangeInput

  override def preStart: Unit = {
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
    self ! StartInteraction
  }

  override def receive: Receive = handshake

  override def postStop(): Unit = log.info(s"Peer handler to $remote destroyed")
}

object PeerConnectionHandler {


  // todo: use the "become" approach to handle state more elegantly
  sealed trait CommunicationState
  case object AwaitingHandshake extends CommunicationState
  case object WorkingCycle extends CommunicationState
  case class Handshaked(peer: PeerInfo)

  object ReceivableMessages {
    private[PeerConnectionHandler] object HandshakeDone
    case object StartInteraction
    case object HandshakeTimeout
    case object CloseConnection
    case object Blacklist
  }
}

object PeerConnectionHandlerRef {
  def props(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            messagesHandler: MessageHandler,
            handshakeSerializer: HandshakeSerializer,
            connectionDescription: ConnectionDescription,
            timeProvider: TimeProvider)(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionHandler(settings, networkControllerRef, peerManagerRef, messagesHandler,
                                    handshakeSerializer, connectionDescription, timeProvider))

  def apply(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            messagesHandler: MessageHandler,
            handshakeSerializer: HandshakeSerializer,
            connectionDescription: ConnectionDescription,
            timeProvider: TimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, messagesHandler, handshakeSerializer,
                         connectionDescription, timeProvider))

  def apply(name: String,
            settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            messagesHandler: MessageHandler,
            handshakeSerializer: HandshakeSerializer,
            connectionDescription: ConnectionDescription,
            timeProvider: TimeProvider)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, messagesHandler, handshakeSerializer,
                         connectionDescription, timeProvider), name)
}
