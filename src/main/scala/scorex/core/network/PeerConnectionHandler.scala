package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import scorex.core.app.{ScorexContext, Version}
import scorex.core.network.NetworkController.ReceivableMessages.Handshaked
import scorex.core.network.PeerConnectionHandler.ReceivableMessages
import scorex.core.network.PeerFeature.Serializers
import scorex.core.network.message.{HandshakeSpec, MessageSerializer}
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddToBlacklist, RemovePeer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


sealed trait ConnectionType

case object Incoming extends ConnectionType

case object Outgoing extends ConnectionType

/**
  * Peer connected to our node
  *
  * @param remote     - connection address
  * @param handlerRef - reference to PeerConnectionHandler that is responsible for communication with this peer
  * @param peerInfo   - information about this peer. May be None if peer is connected, but is not handshaked yet
  */
case class ConnectedPeer(remote: InetSocketAddress,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  override def hashCode(): Int = remote.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case p: ConnectedPeer => p.remote == this.remote && peerInfo == this.peerInfo
    case _ => false
  }

  override def toString: String = s"ConnectedPeer($remote)"
}

case class ConnectionDescription(connection: ActorRef,
                                 direction: ConnectionType,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 remote: InetSocketAddress,
                                 localFeatures: Seq[PeerFeature])

class PeerConnectionHandler(val settings: NetworkSettings,
                            networkControllerRef: ActorRef,
                            peerManagerRef: ActorRef,
                            scorexContext: ScorexContext,
                            connectionDescription: ConnectionDescription
                           )(implicit ec: ExecutionContext)
  extends Actor with ScorexLogging {

  import PeerConnectionHandler.ReceivableMessages._

  val daRef: ActorRef = ???

  private val connection = connectionDescription.connection
  private val direction = connectionDescription.direction
  private val ownSocketAddress = connectionDescription.ownSocketAddress
  private val remote = connectionDescription.remote
  private val localFeatures = connectionDescription.localFeatures

  private val featureSerializers: Serializers = {
    localFeatures.map(f => f.featureId -> (f.serializer: ScorexSerializer[_ <: PeerFeature])).toMap
  }

  private val handshakeSerializer = new HandshakeSpec(featureSerializers, settings.maxHandshakeSize)
  private val messageSerializer = new MessageSerializer(scorexContext.messageSpecs, settings.magicBytes)

  // there is no recovery for broken connections
  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var selfPeer: Option[ConnectedPeer] = None

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var chunksBuffer: ByteString = CompactByteString.empty

  private var outMessagesBuffer: TreeMap[Long, ByteString] = TreeMap.empty

  private var outMessagesCounter: Long = 0

  override def preStart: Unit = {
    context watch connection
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading

    context.become(handshaking)
  }

  override def receive: Receive = reportStrangeInput

  override def postStop(): Unit = log.info(s"Peer handler to $remote destroyed")

  private def handshaking: Receive = {
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    val hb = handshakeSerializer.toBytes(createHandshakeMessage())
    connection ! Tcp.Write(ByteString(hb))
    log.info(s"Handshake sent to $remote")

    receiveAndHandleHandshake { receivedHandshake =>
      log.info(s"Got a Handshake from $remote")

      val peerInfo = PeerInfo(
        receivedHandshake.peerSpec,
        scorexContext.timeProvider.time(),
        Some(direction)
      )
      val peer = ConnectedPeer(remote, self, Some(peerInfo))
      selfPeer = Some(peer)

      networkControllerRef ! Handshaked(peerInfo)
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycleWriting
    } orElse handshakeTimeout orElse fatalCommands
  }

  private def receiveAndHandleHandshake(handler: Handshake => Unit): Receive = {
    case Received(data) =>
      handshakeSerializer.parseBytesTry(data.toArray) match {
        case Success(handshake) =>
          handler(handshake)

        case Failure(t) =>
          log.info(s"Error during parsing a handshake", t)
          //todo: blacklist?
          selfPeer.foreach(c => peerManagerRef ! RemovePeer(c.remote))
          self ! CloseConnection
      }
  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $remote, going to drop the connection")
      self ! CloseConnection
  }

  private def workingCycleWriting: Receive =
    localInterfaceWriting orElse
      remoteInterface orElse
      fatalCommands orElse
      reportStrangeInput

  private def workingCycleBuffering: Receive =
    localInterfaceBuffering orElse
      remoteInterface orElse
      fatalCommands orElse
      reportStrangeInput

  private def fatalCommands: Receive = {
    case _: ConnectionClosed =>
      log.info(s"Connection closed to $remote")
      context stop self

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
      peerManagerRef ! AddToBlacklist(remote)
      connection ! Close
  }

  def localInterfaceWriting: Receive = {
    case msg: message.Message[_] =>
      log.info("Send message " + msg.spec + " to " + remote)
      outMessagesCounter += 1
      connection ! Write(messageSerializer.serialize(msg), ReceivableMessages.Ack(outMessagesCounter))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      log.warn(s"Failed to write ${msg.length} bytes to $remote, switching to buffering mode")
      connection ! ResumeWriting
      buffer(id, msg)
      context become workingCycleBuffering

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + ", switching to closing mode")
      if (outMessagesBuffer.isEmpty) connection ! Close else context become closingWithNonEmptyBuffer

    case ReceivableMessages.Ack(_) => // ignore ACKs in stable mode

    case WritingResumed => // ignore in stable mode
  }

  // operate in ACK mode until all buffered messages are transmitted
  def localInterfaceBuffering: Receive = {
    case msg: message.Message[_] =>
      outMessagesCounter += 1
      buffer(outMessagesCounter, messageSerializer.serialize(msg))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      connection ! ResumeWriting
      buffer(id, msg)

    case CommandFailed(ResumeWriting) => // ignore in ACK mode

    case WritingResumed =>
      writeFirst()

    case ReceivableMessages.Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.nonEmpty) writeFirst()
      else {
        log.info("Buffered messages processed, exiting buffering mode")
        context become workingCycleWriting
      }

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + s", switching to closing mode")
      writeAll()
      context become closingWithNonEmptyBuffer
  }

  def remoteInterface: Receive = {
    case Received(data) =>

      chunksBuffer ++= data

      @tailrec
      def process(): Unit = {
        messageSerializer.deserialize(chunksBuffer, selfPeer) match {
          case Success(Some(message)) =>
            log.info("Received message " + message.spec + " from " + remote)
            networkControllerRef ! message
            chunksBuffer = chunksBuffer.drop(message.messageLength)
            process()
          case Success(None) =>
          case Failure(e) => log.info(s"Corrupted data from ${remote.toString}: ${e.getMessage}")
        }
      }

      process()
      connection ! ResumeReading
  }

  def closingWithNonEmptyBuffer: Receive = {
    case CommandFailed(_: Write) =>
      connection ! ResumeWriting
      context.become({
        case WritingResumed =>
          writeAll()
          context.unbecome()
        case ReceivableMessages.Ack(id) =>
          outMessagesBuffer -= id
      }, discardOld = false)

    case ReceivableMessages.Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.isEmpty) connection ! Close

    case other =>
      log.debug(s"Got $other in closing phase")
  }

  private def reportStrangeInput: Receive = {
    case nonsense =>
      log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  private def buffer(id: Long, msg: ByteString): Unit = {
    outMessagesBuffer += id -> msg
  }

  private def writeFirst(): Unit = {
    outMessagesBuffer.headOption.foreach { case (id, msg) =>
      connection ! Write(msg, ReceivableMessages.Ack(id))
    }
  }

  private def writeAll(): Unit = {
    outMessagesBuffer.foreach { case (id, msg) =>
      connection ! Write(msg, ReceivableMessages.Ack(id))
    }
  }

  private def createHandshakeMessage() = {
    Handshake(PeerSpec(settings.agentName,
      Version(settings.appVersion),
      settings.nodeName,
      ownSocketAddress,
      localFeatures),
      scorexContext.timeProvider.time()
    )
  }

}

object PeerConnectionHandler {

  object ReceivableMessages {

    private[PeerConnectionHandler] object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

    case object Blacklist

    final case class Ack(id: Long) extends Tcp.Event

  }

}

object PeerConnectionHandlerRef {
  def props(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            connectionDescription: ConnectionDescription
           )(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionHandler(settings, networkControllerRef, peerManagerRef, scorexContext, connectionDescription))

  def apply(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            connectionDescription: ConnectionDescription)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, scorexContext, connectionDescription))

  def apply(name: String,
            settings: NetworkSettings,
            networkControllerRef: ActorRef,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            connectionDescription: ConnectionDescription)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, peerManagerRef, scorexContext, connectionDescription), name)
}