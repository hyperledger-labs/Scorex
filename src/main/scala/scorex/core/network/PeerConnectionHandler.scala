package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import scorex.core.app.Version
import scorex.core.network.PeerConnectionHandler.{AwaitingHandshake, WorkingCycle}
import scorex.core.network.message.MessageHandler
import scorex.core.settings.NetworkSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}


sealed trait ConnectionType
case object Incoming extends ConnectionType
case object Outgoing extends ConnectionType


case class ConnectedPeer(socketAddress: InetSocketAddress,
                         handlerRef: ActorRef,
                         direction: ConnectionType,
                         handshake: Handshake) {

  import shapeless.syntax.typeable._

  def publicPeer: Boolean = handshake.declaredAddress.contains(socketAddress)

  override def hashCode(): Int = socketAddress.hashCode()

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(p => p.socketAddress == this.socketAddress && p.direction == this.direction)

  override def toString: String = s"ConnectedPeer($socketAddress)"
}


case object Ack extends Event


class PeerConnectionHandler(val settings: NetworkSettings,
                            networkControllerRef: ActorRef,
                            peerManagerRef: ActorRef,
                            messagesHandler: MessageHandler,
                            connection: ActorRef,
                            direction: ConnectionType,
                            ownSocketAddress: Option[InetSocketAddress],
                            remote: InetSocketAddress,
                            timeProvider: NetworkTimeProvider) extends Actor with Buffering with ScorexLogging {

  import PeerConnectionHandler.ReceivableMessages._
  import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddToBlacklist, Handshaked, Disconnected, DoConnecting}

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
      peerManagerRef ! Disconnected(remote)
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
      val hb = Handshake(settings.agentName,
        Version(settings.appVersion), settings.nodeName,
        ownSocketAddress, timeProvider.time()).bytes

      connection ! Tcp.Write(ByteString(hb))
      log.info(s"Handshake sent to $remote")
      handshakeSent = true
      if (handshakeGot && handshakeSent) self ! HandshakeDone
  }

  private def receivedData: Receive = {
    case Received(data) =>
      HandshakeSerializer.parseBytes(data.toArray) match {
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

      val peer = ConnectedPeer(remote, self, direction, receivedHandshake.get)
      selfPeer = Some(peer)

      peerManagerRef ! Handshaked(peer)
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
        messagesHandler.parseBytes(packet.toByteBuffer, Some(selfPeer.get)) match {   //todo: .get
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
    peerManagerRef ! DoConnecting(remote, direction)
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading
  }

  override def receive: Receive = handshake

  override def postStop(): Unit = log.info(s"Peer handler to $remote destroyed")
}

object PeerConnectionHandler {


  // todo: use the "become" approach to handle state more elegantly
  sealed trait CommunicationState
  case object AwaitingHandshake extends CommunicationState
  case object WorkingCycle extends CommunicationState

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
            connection: ActorRef,
            direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress],
            remote: InetSocketAddress,
            timeProvider: NetworkTimeProvider): Props =
    Props(new PeerConnectionHandler(settings, networkControllerRef, peerManagerRef, messagesHandler,
                                    connection, direction, ownSocketAddress, remote, timeProvider))

  def apply(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            messagesHandler: MessageHandler,
            connection: ActorRef,
            direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress],
            remote: InetSocketAddress,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, messagesHandler,
                         connection, direction, ownSocketAddress, remote, timeProvider))

  def apply(name: String,
            settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            messagesHandler: MessageHandler,
            connection: ActorRef,
            direction: ConnectionType,
            ownSocketAddress: Option[InetSocketAddress],
            remote: InetSocketAddress,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, messagesHandler,
                         connection, direction, ownSocketAddress, remote, timeProvider), name)
}
