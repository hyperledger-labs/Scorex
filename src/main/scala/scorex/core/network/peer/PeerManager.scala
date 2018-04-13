package scorex.core.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.core.network._
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}
import PeerManager.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import scorex.core.network.NetworkController.ReceivableMessages.StartConnecting
import scorex.core.network.PeerConnectionHandler.ReceivableMessages.{StartInteraction, CloseConnection}

import scala.collection.mutable
import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: ScorexSettings, timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  //peers after successful handshake
  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()

  //peers before handshake
  private val connectingPeers = mutable.Set[InetSocketAddress]()

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

  private def peerCycle: Receive =
    peerHandlerMessages orElse
      receiveHandshaked orElse
      receiveDisconnected

  private def peerHandlerMessages: Receive = {
    // Only PeerHandler should send this message
    // todo: think about refactoring
    case DoConnecting(remote, direction) =>
      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        val peerHandlerRef = sender
        val isIncoming = direction == Incoming
        val isConnecting = connectingPeers.contains(remote)
        if (isConnecting || isIncoming) {
          if (!isIncoming) log.info(s"Connecting to $remote")
          peerHandlerRef ! StartInteraction
          lastIdUsed += 1
        } else {
          log.info(s"Unknown peer $remote trying to connect, going to drop the connection")
          peerHandlerRef ! CloseConnection
        }
      }
  }

  private def receiveHandshaked: Receive = {
    case Handshaked(peer) =>
      //todo: filter by an id introduced by the PeerManager
      if (peerDatabase.isBlacklisted(peer.socketAddress)) {
        log.info(s"Got handshake from blacklisted ${peer.socketAddress}")
      } else {
        //drop connection to self if occurred
        if (peer.direction == Outgoing && isSelf(peer.socketAddress, peer.handshake.declaredAddress)) {
          peer.handlerRef ! CloseConnection
        } else {
          if (peer.publicPeer) self ! AddOrUpdatePeer(peer.socketAddress, Some(peer.handshake.nodeName), Some(peer.direction))
          connectedPeers += peer.socketAddress -> peer
          context.system.eventStream.publish(HandshakedPeer(peer))
        }
      }
  }

  private def receiveDisconnected: Receive = {
    case Disconnected(remote) =>
      connectedPeers -= remote
      connectingPeers -= remote
      context.system.eventStream.publish(DisconnectedPeer(remote))
  }

  override def receive: Receive =
    networkControllerMessages orElse
      receiveAddToBlackList orElse
      peerListOperations orElse
      apiInterface orElse
      peerCycle


  private def networkControllerMessages: Receive = {
    // Warning: We expect that only NetworkController should send these messages.
    // todo: Refactoring needed for these two actors to achieve more transparent code
    case ConnectNonDuplicate(remote) =>
      val networkController = sender
      if (checkNotConnected(remote)) {
        startConnecting(remote, networkController)
      } else {
        log.info(s"Already connected peer $remote trying to connect, discarding")
      }
    case CheckPeers =>
      val networkController = sender
      if (connectedPeers.size + connectingPeers.size < settings.network.maxConnections) {
        randomPeer().foreach { address =>
          //todo: avoid picking too many peers from the same bucket, see Bitcoin ref. impl.
          if (checkNotConnected(address)) {
            startConnecting(address, networkController)
          }
        }
      }
  }

  private def checkNotConnected(address: InetSocketAddress): Boolean = {
    !connectedPeers.exists(_._1 == address) && !connectingPeers.exists(_.getHostName == address.getHostName)
  }

  private def startConnecting(address: InetSocketAddress, networkController: ActorRef): Unit = {
    connectingPeers += address
    networkController ! StartConnecting(address)
  }

  private def receiveAddToBlackList: Receive = {
    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer, timeProvider.time())
    // todo: shouldn't peer be removed from `connectedPeers` when it is blacklisted?
  }

}

object PeerManager {

  object ReceivableMessages {

    case object CheckPeers // Only NetworkController should send this
    case class ConnectNonDuplicate(remote: InetSocketAddress) // Only NetworkController should send this

    case class AddToBlacklist(remote: InetSocketAddress)

    // peerListOperations messages
    case class AddOrUpdatePeer(address: InetSocketAddress, peerName: Option[String], direction: Option[ConnectionType])
    case object KnownPeers
    case object RandomPeer
    case class RandomPeers(howMany: Int)
    case class FilterPeers(sendingStrategy: SendingStrategy)

    // apiInterface messages
    case object GetConnectedPeers
    case object GetAllPeers
    case object GetBlacklistedPeers

    // peerCycle messages
    case class DoConnecting(remote: InetSocketAddress, direction: ConnectionType) // Only PeerHandler should send this
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
