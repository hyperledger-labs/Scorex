package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, NetworkUtils, ScorexLogging, TimeProvider}

import scala.collection.mutable
import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, timeProvider: TimeProvider,
                  externalNodeAddress: Option[InetSocketAddress]) extends Actor with ScorexLogging {

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
      if (!isSelf(address)) {
        val defaultPeerInfo = PeerInfo(timeProvider.time(), address, None, None, Seq())
        peerDatabase.addOrUpdateKnownPeer(defaultPeerInfo)
      }
    }
  }

  private def notConnectedPeers = {
    peerDatabase.knownPeers().filterNot { case (addr, _) =>
      val connected = connectedPeers.values.exists(p =>
        p.localAddress.contains(addr) || p.handshake.declaredAddress.contains(addr)
      )
      connected || connectingPeers.contains(addr)
    }.values.toSeq
  }

  private def randomPeer(): Option[PeerInfo] = {
    val peers = notConnectedPeers
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNameOpt, connTypeOpt, features) =>
      if (!isSelf(address)) {
        val peerInfo = PeerInfo(timeProvider.time(), address, peerNameOpt, connTypeOpt, features)
        peerDatabase.addOrUpdateKnownPeer(peerInfo)
      }

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers().keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers().values.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.values.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.map(_.handshake).toSeq: Seq[Handshake])

    case GetAllPeers =>
      log.trace(s"Get all peers: ${peerDatabase.knownPeers()}")
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
    * Given a peer's address, returns `true` if the peer is the same is this node.
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, settings.network.bindAddress, externalNodeAddress)
  }

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
        }
      }
  }


  private def handshaked: Receive = {
    case Handshaked(peer) =>
      //todo: filter by an id introduced by the PeerManager
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
      } else {
        log.trace(s"Got handshake from $peer")
        //drop connection to self if occurred
        if (peer.direction == Outgoing && isSelf(peer.socketAddress)) {
          peer.handlerRef ! CloseConnection
        } else {
          if (peer.reachablePeer) {
            val peerName = Some(peer.handshake.nodeName)
            val peerFeats = peer.handshake.features
            val address = peer.handshake.declaredAddress.orElse(peer.localAddress).getOrElse(peer.socketAddress)
            self ! AddOrUpdatePeer(address, peerName, Some(peer.direction), peerFeats)
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
        randomPeer().foreach { peerInfo =>
          //todo: avoid picking too many peers from the same bucket, see Bitcoin ref. impl.
          val connectedPeersAddrs = connectedPeers.values.flatMap(_.handshake.declaredAddress)
          if (!connectedPeersAddrs.exists(_ == peerInfo.decalerdAddress) /*&&
            !connectedPeersAddrs.exists(_.getHostName == peerInfo.decalerdAddress.getHostName)*/) {

            sender() ! ConnectTo(peerInfo)
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
  def props(settings: ScorexSettings, timeProvider: TimeProvider,
            externalNodeAddress: Option[InetSocketAddress]): Props = {
    Props(new PeerManager(settings, timeProvider, externalNodeAddress))
  }

  def apply(settings: ScorexSettings, timeProvider: TimeProvider,
            externalNodeAddress: Option[InetSocketAddress])
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, timeProvider, externalNodeAddress))
  }

  def apply(name: String, settings: ScorexSettings, timeProvider: TimeProvider,
            externalNodeAddress: Option[InetSocketAddress])
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, timeProvider, externalNodeAddress), name)
  }
}
