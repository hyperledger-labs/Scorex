package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.network.PeerConnectionHandler.CloseConnection
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.collection.mutable
import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import PeerManager._

  //peers after successful handshake
  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //peers before handshake
  private val connectingPeers = mutable.Set[InetSocketAddress]()

  private val subscribers = mutable.Map[PeerManager.EventType.Value, Seq[ActorRef]]()

  protected def notifySubscribers[O <: PeerManagerEvent](eventType: EventType.Value, event: O): Unit =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! event)

  private lazy val peerDatabase = new PeerDatabaseImpl(Some(settings.dataDir + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = PeerInfo(timeProvider.time(), None)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
    }
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers().keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt) =>
      if (!isSelf(address, None)) {
        val peerInfo = PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt)
        peerDatabase.addOrUpdateKnownPeer(address, peerInfo)
      }

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers().keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers().keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.map(_.handshake).toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers()

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()

    case PeerManager.Subscribe(listener, events) =>
      events.foreach { evt =>
        val current = subscribers.getOrElse(evt, Seq())
        subscribers.put(evt, current :+ listener)
      }
  }


  /**
    * Given a peer's address and declared address, returns `true` iff the peer is the same is this node.
    */
  private def isSelf(address: InetSocketAddress, declaredAddress: Option[InetSocketAddress]): Boolean = {
    // TODO: should the peer really be considered the same as self iff one of the following conditions hold?? Check carefully.
    settings.network.bindAddress == address ||
      settings.network.declaredAddress.exists(da => declaredAddress.contains(da)) ||
      declaredAddress.contains(settings.network.bindAddress) ||
      settings.network.declaredAddress.contains(address)
  }

  private var lastIdUsed = 0

  private def peerCycle: Receive = {
    case DoConnecting(remote, direction) =>
      val peerHandlerRef = sender()

      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        val refuse =
          if (direction == Incoming) false
          else if (connectingPeers.contains(remote)) {
            log.info(s"Connecting to $remote")
            false
          } else {
            log.info(s"Already connected peer $remote trying to connect, going to drop the duplicate connection")
            true
          }

        if (refuse) {
          peerHandlerRef ! CloseConnection
        } else {
          peerHandlerRef ! PeerConnectionHandler.StartInteraction
          lastIdUsed += 1
        }
      }

    //todo: filter by an id introduced by the PeerManager
    case h@Handshaked(peer) =>
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
      } else {
          //drop connection to self if occurred
          if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
            peer.handlerRef ! PeerConnectionHandler.CloseConnection
          } else {
            if(peer.publicPeer) self ! PeerManager.AddOrUpdatePeer(peer.socketAddress, Some(peer.handshake.nodeName), Some(peer.direction))
            connectedPeers += peer.socketAddress -> peer
            notifySubscribers(PeerManager.EventType.Handshaked, HandshakedPeer(peer))
          }
        }


    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      notifySubscribers(PeerManager.EventType.Disconnected, PeerManager.DisconnectedPeer(remote))
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          //todo: avoid picking too many peers from the same bucket, see Bitcoin ref. impl.
          if (!connectedPeers.exists(_._1 == address) &&
            !connectingPeers.exists(_.getHostName == address.getHostName)) {
            connectingPeers += address
            sender() ! NetworkController.ConnectTo(address)
          }
        }
      }

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer, timeProvider.time())
      // todo: shouldn't peer be removed from `connectedPeers` when it is blacklisted?
  }: Receive) orElse peerListOperations orElse apiInterface orElse peerCycle
}

object PeerManager {

  object EventType extends Enumeration {
    val Handshaked: EventType.Value = Value(1)
    val Disconnected: EventType.Value = Value(2)
  }

  case class Subscribe(listener: ActorRef, events: Seq[EventType.Value])

  trait PeerManagerEvent

  case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

  case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

  case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)

  case class Handshaked(peer: ConnectedPeer)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers

}

object PeerManagerRef {
  def props(settings: ScorexSettings, timeProvider: NetworkTimeProvider): Props =
    Props(new PeerManager(settings, timeProvider))

  def apply(settings: ScorexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider))

  def apply(name: String, settings: ScorexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider), name)
}
