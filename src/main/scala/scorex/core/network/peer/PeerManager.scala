package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
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

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private val connectingPeers = mutable.Set[InetSocketAddress]()

  private val subscribers = mutable.Map[PeerManager.EventType.Value, Seq[ActorRef]]()

  protected def notifySubscribers[O <: PeerManagerEvent](eventType: EventType.Value, event: O): Unit =
    subscribers.getOrElse(eventType, Seq()).foreach(_ ! event)

  private lazy val peerDatabase = new PeerDatabaseImpl(settings.network, Some(settings.dataDir + "/peers.dat"), timeProvider)

  if (peerDatabase.isEmpty()) {
    settings.network.knownPeers.foreach { address =>
      val defaultPeerInfo = PeerInfo(timeProvider.time(), None, None)
      peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
    }
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers(true).keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      val peerInfo = PeerInfo(timeProvider.time(), peerNonceOpt, peerNameOpt)
      peerDatabase.addOrUpdateKnownPeer(address, peerInfo)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers(false).keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(false).keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers(true)

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


  private def peerCycle: Receive = {
    case Connected(newPeer@ConnectedPeer(remote, _)) =>
      if (peerDatabase.isBlacklisted(newPeer.socketAddress)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        if(!connectedPeers.exists(_._1.socketAddress.getHostName == remote.getHostName)) {
          connectedPeers += newPeer -> None
          if (connectingPeers.contains(remote)) {
            log.info(s"Connected to $remote. ${connectedPeers.size} connections are open")
            connectingPeers -= remote
          } else {
            log.info(s"Got incoming connection from $remote. ${connectedPeers.size} connections are open")
          }
          newPeer.handlerRef ! PeerConnectionHandler.StartInteraction
        } else {
          log.info(s"Already connected peer $remote trying to connect, going to disconnect it")
          newPeer.handlerRef ! CloseConnection
        }
      }

    case h@Handshaked(address, handshake) =>
      if (peerDatabase.isBlacklisted(address)) {
        log.info(s"Got handshake from blacklisted $address")
      } else {
        val toUpdate = connectedPeers.filter { case (cp, h) =>
          // TODO: Replaced `nonce` by `declaredAddress`. Is this really what we want? Check carefully.
          cp.socketAddress == address || h.forall(_.declaredAddress == handshake.declaredAddress)
        }

        if (toUpdate.isEmpty) {
          log.error("No peer to update")
        } else {
          val newCp = toUpdate
            .find(t => handshake.declaredAddress.contains(t._1.socketAddress))
            .getOrElse(toUpdate.head)
            ._1

          toUpdate.keys.foreach(connectedPeers.remove)

          //drop connection to self if occurred
          if (isSelf(address, handshake.declaredAddress)) {
            newCp.handlerRef ! PeerConnectionHandler.CloseConnection
          } else {
            handshake.declaredAddress.foreach(address => self ! PeerManager.AddOrUpdatePeer(address, None, None))
            connectedPeers += newCp -> Some(handshake)
            notifySubscribers(PeerManager.EventType.Handshaked, HandshakedPeer(newCp))
          }
        }
      }

    case Disconnected(remote) =>
      connectedPeers.retain { case (p, _) => p.socketAddress != remote }
      if (connectingPeers.contains(remote)) connectingPeers -= remote
      notifySubscribers(PeerManager.EventType.Disconnected, PeerManager.DisconnectedPeer(remote))
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          //todo: avoid picking too many peers from the same bucket, see Bitcoin ref. impl.
          if (!connectedPeers.exists(_._1.socketAddress.getHostName == address.getHostName) &&
              !connectingPeers.exists(_.getHostName == address.getHostName)) {
            connectingPeers += address
            sender() ! NetworkController.ConnectTo(address)
          }
        }
      }

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer)
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

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class Connected(newPeer: ConnectedPeer)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers
}
