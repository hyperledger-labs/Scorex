package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import PeerManager.ReceivableMessages._
  import scorex.core.network.NetworkController.ReceivableMessages.ConnectTo
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
  import scorex.core.network.PeerConnectionHandler.ReceivableMessages.{CloseConnection, StartInteraction}

  //peers after successful handshake
  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //peers before handshake
  private val connectingPeers = mutable.Set[InetSocketAddress]()

  private lazy val peerDatabase = new PeerDatabaseImpl(Some(settings.dataDir + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address, None)) {
        val defaultPeerInfo = PeerInfo(timeProvider.time(), None, None, Seq())
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
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt, features) =>
      if (!isSelf(address, None)) {
        val peerInfo = PeerInfo(timeProvider.time(), peerNameOpt, connTypeOpt, features)
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

//    case Subscribe(listener, events) =>
//      events.foreach { evt =>
//        val current = subscribers.getOrElse(evt, Seq())
//        subscribers.put(evt, current :+ listener)
//      }
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

  private def peerCycle: Receive = connecting orElse handshaked orElse disconnected

  private def connecting: Receive = {
    case DoConnecting(remote, direction) =>
      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        val peerHandlerRef = sender
        val isIncoming = direction == Incoming
        val isAlreadyConnecting = connectingPeers.contains(remote)
        if (isAlreadyConnecting && !isIncoming) {
          log.info(s"Trying to connect twice to $remote, going to drop the duplicate connection")
          peerHandlerRef ! CloseConnection
        } else {
          if (!isIncoming) {
            log.info(s"Connecting to $remote")
            connectingPeers += remote
          }
          peerHandlerRef ! StartInteraction
          lastIdUsed += 1
        }
      }
  }


  private def handshaked: Receive = {
    case Handshaked(peer) =>
      //todo: filter by an id introduced by the PeerManager
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
      } else {
        //drop connection to self if occurred
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
          peer.handlerRef ! CloseConnection
        } else {
          if (peer.publicPeer) {
            val peerName = Some(peer.handshake.nodeName)
            val peerFeats = peer.handshake.features
            self ! AddOrUpdatePeer(peer.socketAddress, peerName, Some(peer.direction), peerFeats)
          } else {
            peerDatabase.remove(peer.socketAddress)
          }
          connectedPeers += peer.socketAddress -> peer
          context.system.eventStream.publish(HandshakedPeer(peer))
        }
      }
  }

  private def disconnected: Receive = {
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      context.system.eventStream.publish(DisconnectedPeer(remote))
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          //todo: avoid picking too many peers from the same bucket, see Bitcoin ref. impl.
          if (!connectedPeers.exists(_._1 == address) &&
            !connectingPeers.exists(_.getHostName == address.getHostName)) {
            sender() ! ConnectTo(address)
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

  object ReceivableMessages {
    case object CheckPeers
    case class AddToBlacklist(remote: InetSocketAddress)

    // peerListOperations messages
    case class AddOrUpdatePeer(address: InetSocketAddress,
                               peerName: Option[String],
                               direction: Option[ConnectionType],
                               features: Seq[PeerFeature])
    case object KnownPeers
    case object RandomPeer
    case class RandomPeers(hawMany: Int)
    case class FilterPeers(sendingStrategy: SendingStrategy)

    // apiInterface messages
    case object GetConnectedPeers
    case object GetAllPeers
    case object GetBlacklistedPeers

    // peerCycle messages
    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType)
    case class Handshaked(peer: ConnectedPeer)
    case class Disconnected(remote: InetSocketAddress)
  }
}

object PeerManagerRef {
  def props(settings: ScorexSettings, timeProvider: NetworkTimeProvider): Props =
    Props(new PeerManager(settings, timeProvider))

  def apply(settings: ScorexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider))

  def apply(name: String, settings: ScorexSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(settings, timeProvider), name)
}
