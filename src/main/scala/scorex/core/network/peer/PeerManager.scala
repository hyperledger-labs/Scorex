package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.app.{ScorexContext, Version}
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkUtils
import scorex.util.ScorexLogging

import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, scorexContext: ScorexContext) extends Actor with ScorexLogging {

  import PeerManager.ReceivableMessages._

  private lazy val peerDatabase = new PeerDatabaseImpl(Some(settings.dataDir + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address)) {
        // fill database with peers from config file if empty
        peerDatabase.addOrUpdateKnownPeer(PeerInfo.fromAddress(address, scorexContext.timeProvider.time()))
      }
    }
  }

  override def receive: Receive = ({

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer, scorexContext.timeProvider.time())
    // todo: shouldn't peer be removed from `connectedPeers` when it is blacklisted?
  }: Receive) orElse peerListOperations orElse apiInterface

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(peerData) =>
      if (!(peerData.declaredAddress.exists(isSelf) || peerData.localAddressOpt.exists(isSelf))) {
        // todo
        val currentTime = System.currentTimeMillis()
        val peerInfo = PeerInfo(peerData, currentTime, None)
        peerDatabase.addOrUpdateKnownPeer(peerInfo)
      }

    case RemovePeer(address) =>
      peerDatabase.remove(address)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers().keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers().values.toSeq).take(howMany)

    case RandomPeerExcluding(excludedPeers) =>
      sender() ! randomPeerExcluded(excludedPeers)
  }

  private def apiInterface: Receive = {

    case GetAllPeers =>
      log.trace(s"Get all peers: ${peerDatabase.knownPeers()}")
      sender() ! peerDatabase.knownPeers()

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()

  }

  /**
    * Given a peer's address, returns `true` if the peer is the same is this node.
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, settings.network.bindAddress, scorexContext.externalNodeAddress)
  }

  private def randomPeer(): Option[PeerInfo] = {
    randomPeer(peerDatabase.knownPeers().values.toSeq)
  }

  private def randomPeerExcluded(excludedPeers: Seq[PeerInfo]): Option[PeerInfo] = {
    val candidates = peerDatabase.knownPeers().values.filterNot { p =>
      excludedPeers.exists(_.peerData.address == p.peerData.address)
    }.toSeq

    randomPeer(candidates)
  }

  private def randomPeer(peers: Seq[PeerInfo]): Option[PeerInfo] = {
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }
}

object PeerManager {

  object ReceivableMessages {

    case class AddToBlacklist(remote: InetSocketAddress)

    // peerListOperations messages
    case class AddOrUpdatePeer(data: PeerData)

    case class RemovePeer(address: InetSocketAddress)

    //todo unused?
    case object KnownPeers

    //todo unused?
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
