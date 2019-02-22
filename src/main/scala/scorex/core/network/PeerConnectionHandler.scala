package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import scorex.core.app.{ScorexContext, Version}
import scorex.core.network.NetworkController.ReceivableMessages.Handshaked
import scorex.core.network.PeerFeature.Serializers
import scorex.core.network.message.{HandshakeSpec, MessageSerializer}
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddToBlacklist, RemovePeer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}


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

case object Ack extends Event


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

  private var chunksBuffer: ByteString = CompactByteString()

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
      context become workingCycle
    } orElse handshakeTimeout orElse processErrors("Handshaking")
  }

  private def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors("WorkingCycle") orElse
      reportStrangeInput

  private def createHandshakeMessage() = {
    Handshake(PeerSpec(settings.agentName,
      Version(settings.appVersion),
      settings.nodeName,
      ownSocketAddress,
      localFeatures),
      scorexContext.timeProvider.time()
    )
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

  private def processErrors(stateName: String): Receive = {
    case c: CommandFailed =>
      c.cmd match {
        case w: Write =>
          log.warn(s"$c: Failed to write ${w.data.length} bytes to $remote in state $stateName")
          //      peerManager ! AddToBlacklist(remote)
          connection ! Close
          connection ! ResumeReading
          connection ! ResumeWriting
        case _ =>
          log.warn(s"$c: Failed to execute command to $remote in state $stateName")
          connection ! ResumeReading
      }

    case cc: ConnectionClosed =>
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context stop self

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close

  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $remote, going to drop the connection")
      self ! CloseConnection
  }

  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
      def sendOutMessage() {
        log.info("Send message " + msg.spec + " to " + remote)
        connection ! Write(messageSerializer.serialize(msg))
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

  private def reportStrangeInput: Receive = {
    case nonsense: Any =>
      log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }
}

object PeerConnectionHandler {

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