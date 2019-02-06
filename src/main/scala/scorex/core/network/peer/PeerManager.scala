package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.app.ScorexContext
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkUtils
import scorex.util.ScorexLogging

import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, scorexContext: ScorexContext)
  extends Actor with ScorexLogging {

  import PeerManager.ReceivableMessages._

  private lazy val peerDatabase = new InMemoryPeerDatabase(settings, scorexContext.timeProvider)

  if (peerDatabase.isEmpty) {
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address)) {
        val defaultPeerInfo = PeerInfo(scorexContext.timeProvider.time(), Some(address), None, None, Seq())
        peerDatabase.addOrUpdateKnownPeer(defaultPeerInfo)
      }
    }
  }

  override def receive: Receive = peerOperations orElse apiInterface

  private def peerOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt, features) =>
      if (!isSelf(address)) {
        val peerInfo = PeerInfo(scorexContext.timeProvider.time(), Some(address), peerNameOpt, connTypeOpt, features)
        peerDatabase.addOrUpdateKnownPeer(peerInfo)
      }

    case AddToBlacklist(peer) =>
      peerDatabase.addToBlacklist(peer)

    case RemovePeer(address) =>
      peerDatabase.remove(address)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers.keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers.values.toSeq).take(howMany)

    case RandomPeerExcluding(excludedPeers) =>
      sender() ! randomPeerExcluded(excludedPeers)
  }

  private def apiInterface: Receive = {

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers

  }

  /**
    * Given a peer's address, returns `true` if the peer is the same is this node.
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, settings.network.bindAddress, scorexContext.externalNodeAddress)
  }

  private def randomPeer(peers: Seq[PeerInfo]): Option[PeerInfo] = {
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def randomPeer(): Option[PeerInfo] = {
    randomPeer(peerDatabase.knownPeers.values.toSeq)
  }

  private def randomPeerExcluded(excludedPeers: Seq[PeerInfo]): Option[PeerInfo] = {
    val candidates = peerDatabase.knownPeers.values.filterNot { p =>
      excludedPeers.exists(e =>
        e.declaredAddress == p.declaredAddress || (e.localAddress.isDefined && e.localAddress == p.localAddress)
      )
    }.toSeq

    randomPeer(candidates)
  }

}

object PeerManager {

  object ReceivableMessages {
    case class AddToBlacklist(remote: InetSocketAddress)

    // peerOperations messages
    case class AddOrUpdatePeer(address: InetSocketAddress,
                               peerName: Option[String],
                               direction: Option[ConnectionType],
                               features: Seq[PeerFeature])
    case class RemovePeer(address: InetSocketAddress)
    case object KnownPeers
    case object RandomPeer
    case class RandomPeers(hawMany: Int)
    case class RandomPeerExcluding(excludedPeers: Seq[PeerInfo])

    // apiInterface messages
    case object GetAllPeers
    case object GetBlacklistedPeers
  }

}

object PeerManagerRef {

  def props(settings: ScorexSettings, scorexContext: ScorexContext): Props = {
    Props(new PeerManager(settings, scorexContext))
  }

  def apply(settings: ScorexSettings, scorexContext: ScorexContext)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, scorexContext))
  }

  def apply(name: String, settings: ScorexSettings, scorexContext: ScorexContext)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, scorexContext), name)
  }

}
