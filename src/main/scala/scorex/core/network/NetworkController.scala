package scorex.core.network

import java.net._

import akka.actor._
import akka.io.Tcp.SO.KeepAlive
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.app.{ScorexContext, Version}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.{Message, MessageSpec}
import scorex.core.network.peer.PeerManager.ReceivableMessages._
import scorex.core.network.peer.{LocalAddressPeerFeature, PeerInfo, PeerManager}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkUtils
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Try}

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(settings: NetworkSettings,
                        peerManagerRef: ActorRef,
                        scorexContext: ScorexContext,
                        tcpManager: ActorRef
                       )(implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  import NetworkController.ReceivableMessages._
  import NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
  import PeerConnectionHandler.ReceivableMessages.CloseConnection
  import akka.actor.SupervisorStrategy._

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(
      maxNrOfRetries = NetworkController.ChildActorHandlingRetriesNr,
      withinTimeRange = 1.minute) {
        case _: ActorKilledException => Stop
        case _: DeathPactException => Stop
        case e: ActorInitializationException =>
          log.warn(s"Stopping child actor failed with: $e")
          Stop
        case e: Exception =>
          log.warn(s"Restarting child actor failed with: $e")
          Restart
      }

  private implicit val system: ActorSystem = context.system

  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))

  private var messageHandlers = Map.empty[MessageCode, ActorRef]

  private lazy val bindAddress = settings.bindAddress

  private var connections = Map.empty[InetSocketAddress, ConnectedPeer]
  private var unconfirmedConnections = Set.empty[InetSocketAddress]

  //todo: make usage more clear, now we're relying on preStart logic in a actor which is described by a never used val
  private val featureSerializers: PeerFeature.Serializers = scorexContext.features.map(f => f.featureId -> f.serializer).toMap
  private val peerSynchronizer: ActorRef = PeerSynchronizerRef("PeerSynchronizer", self, peerManagerRef, settings,
    featureSerializers)

  //check own declared address for validity
  validateDeclaredAddress()

  log.info(s"Declared address: ${scorexContext.externalNodeAddress}")

  //bind to listen incoming connections
  tcpManager ! Bind(self, bindAddress, options = KeepAlive(true) :: Nil, pullMode = false)

  private def bindingLogic: Receive = {
    case Bound(_) =>
      log.info("Successfully bound to the port " + settings.bindAddress.getPort)
      scheduleConnectionToPeer()

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.bindAddress.getPort + " already in use!")
      context stop self
  }

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytesTry(msgBytes) match {
        case Success(content) =>
          messageHandlers.get(msgId) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error(s"No handlers found for message $remote: " + msgId)
          }
        case Failure(e) =>
          log.error(s"Failed to deserialize data from $remote: ", e)
          blacklist(remote, PenaltyType.PermanentPenalty)
      }

    case SendToNetwork(message, sendingStrategy) =>
      filterConnections(sendingStrategy, message.spec.protocolVersion).foreach { connectedPeer =>
        connectedPeer.handlerRef ! message
      }
  }

  def peerLogic: Receive = {
    case ConnectTo(peer) =>
      connectTo(peer)

    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.connectionId}")
      peer.handlerRef ! CloseConnection

    case BlacklistPeer(peerAddress, penaltyType) =>
      blacklist(peerAddress, penaltyType)

    case Connected(remoteAddress, localAddress) if connectionForPeerAddress(remoteAddress).isEmpty =>
      val connectionDirection: ConnectionDirection =
        if (unconfirmedConnections.contains(remoteAddress)) Outgoing else Incoming
      val connectionId = ConnectionId(remoteAddress, localAddress, connectionDirection)
      log.info(s"Unconfirmed connection: ($remoteAddress, $localAddress) => $connectionId")
      if (connectionDirection.isOutgoing) createPeerConnectionHandler(connectionId, sender())
      else peerManagerRef ! ConfirmConnection(connectionId, sender())

    case Connected(remoteAddress, _) =>
      log.warn(s"Connection to peer $remoteAddress is already established")
      sender() ! Close

    case ConnectionConfirmed(connectionId, handlerRef) =>
      log.info(s"Connection confirmed to $connectionId")
      createPeerConnectionHandler(connectionId, handlerRef)

    case ConnectionDenied(connectionId, handlerRef) =>
      log.info(s"Incoming connection from ${connectionId.remoteAddress} denied")
      handlerRef ! Close

    case Handshaked(connectedPeer) =>
      handleHandshake(connectedPeer, sender())

    case f@CommandFailed(c: Connect) =>
      unconfirmedConnections -= c.remoteAddress
      f.cause match {
        case Some(t) => log.info("Failed to connect to : " + c.remoteAddress, t)
        case None => log.info("Failed to connect to : " + c.remoteAddress)
      }
      // remove not responding peer from database
      peerManagerRef ! RemovePeer(c.remoteAddress)

    case Terminated(ref) =>
      connectionForHandler(ref).foreach { connectedPeer =>
        val remoteAddress = connectedPeer.connectionId.remoteAddress
        connections -= remoteAddress
        unconfirmedConnections -= remoteAddress
        context.system.eventStream.publish(DisconnectedPeer(remoteAddress))
      }
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case GetConnectedPeers =>
      sender() ! connections.values.flatMap(_.peerInfo).toSeq

    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      filterConnections(Broadcast, Version.initial).foreach { connectedPeer =>
        connectedPeer.handlerRef ! CloseConnection
      }
      self ! Unbind
      context stop self
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessageSpecs(specs, handler) =>
      log.info(s"Registering handlers for ${specs.map(s => s.messageCode -> s.messageName)}")
      messageHandlers ++= specs.map(_.messageCode -> handler)

    case CommandFailed(cmd: Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  /**
    * Schedule a periodic connection to a random known peer
    */
  private def scheduleConnectionToPeer(): Unit = {
    context.system.scheduler.schedule(5.seconds, 5.seconds) {
      if (connections.size < settings.maxConnections) {
        val randomPeerF = peerManagerRef ? RandomPeerExcluding(connections.values.flatMap(_.peerInfo).toSeq)
        randomPeerF.mapTo[Option[PeerInfo]].foreach { peerInfoOpt =>
          peerInfoOpt.foreach(peerInfo => self ! ConnectTo(peerInfo))
        }
      }
    }
  }

  /**
    * Connect to peer
    *
    * @param peer - PeerInfo
    */
  private def connectTo(peer: PeerInfo): Unit = {
    log.info(s"Connecting to peer: $peer")
    getPeerAddress(peer) match {
      case Some(remote) =>
        if (connectionForPeerAddress(remote).isEmpty && !unconfirmedConnections.contains(remote)) {
          unconfirmedConnections += remote
          tcpManager ! Connect(
            remoteAddress = remote,
            options = KeepAlive(true) :: Nil,
            timeout = Some(settings.connectionTimeout),
            pullMode = true
          )
        } else {
          log.warn(s"Connection to peer $remote is already established")
        }
      case None =>
        log.warn(s"Can't obtain remote address for peer $peer")
    }
  }

  /**
    * Creates a PeerConnectionHandler for the established connection
    *
    * @param connectionId - connection detailed info
    * @param connection - connection ActorRef
    */
  private def createPeerConnectionHandler(connectionId: ConnectionId,
                                          connection: ActorRef): Unit = {
    val logMsg = connectionId.direction match {
      case Incoming =>
        s"New incoming connection from ${connectionId.localAddress} established (bound to local ${connectionId.remoteAddress})"
      case Outgoing =>
        s"New outgoing connection to ${connectionId.remoteAddress} established (bound to local ${connectionId.localAddress})"
    }
    log.info(logMsg)

    val isLocal = connectionId.remoteAddress.getAddress.isSiteLocalAddress ||
      connectionId.remoteAddress.getAddress.isLoopbackAddress
    val peerFeatures =
      if (isLocal) scorexContext.features :+ LocalAddressPeerFeature(
        new InetSocketAddress(connectionId.localAddress.getAddress, settings.bindAddress.getPort))
      else scorexContext.features

    val connectionDescription = ConnectionDescription(
      connection, connectionId, getNodeAddressForPeer(connectionId.localAddress), peerFeatures)

    val handlerProps: Props = PeerConnectionHandlerRef.props(settings, self, peerManagerRef,
      scorexContext, connectionDescription)

    val handler = context.actorOf(handlerProps) // launch connection handler
    context.watch(handler)
    val connectedPeer = ConnectedPeer(connectionId, handler, None)
    connections += connectionId.remoteAddress -> connectedPeer
    unconfirmedConnections -= connectionId.remoteAddress
  }

  private def handleHandshake(peerInfo: PeerInfo, peerHandler: ActorRef): Unit = {
    def dropConnection(connectedPeer: ConnectedPeer, peerAddress: InetSocketAddress): Unit = {
      log.info(s"Dropping connection to ${connectedPeer.connectionId}")
      connectedPeer.handlerRef ! CloseConnection
      peerManagerRef ! RemovePeer(peerAddress)
      connections -= connectedPeer.connectionId.remoteAddress
    }
    connectionForHandler(peerHandler).foreach { connectedPeer =>
      log.info(s"Checking connected peer: ${connectedPeer.connectionId}")
      val remoteAddress = connectedPeer.connectionId.remoteAddress
      val peerAddress = peerInfo.peerSpec.address.getOrElse(remoteAddress)

      //drop connection to self if occurred or peer already connected
      val shouldDrop = isSelf(remoteAddress) ||
        connectionForPeerAddress(peerAddress).exists(_.handlerRef != peerHandler)
      if (shouldDrop) {
        dropConnection(connectedPeer, peerAddress)
      } else {
        if (peerInfo.peerSpec.reachablePeer) peerManagerRef ! AddOrUpdatePeer(peerInfo)
        else dropConnection(connectedPeer, peerAddress)

        val updatedConnectedPeer = connectedPeer.copy(peerInfo = Some(peerInfo))
        connections += remoteAddress -> updatedConnectedPeer
        context.system.eventStream.publish(HandshakedPeer(updatedConnectedPeer))
      }
    }
  }

  /**
    * Returns connections filtered by given SendingStrategy and Version.
    * Exclude all connections with lower version and apply sendingStrategy to remaining connected peers
    *
    * @param sendingStrategy - SendingStrategy
    * @param version         - minimal version required
    * @return sequence of ConnectedPeer instances according SendingStrategy
    */
  private def filterConnections(sendingStrategy: SendingStrategy, version: Version): Seq[ConnectedPeer] = {
    sendingStrategy.choose(connections.values.toSeq.filter(_.peerInfo.exists(_.peerSpec.protocolVersion >= version)))
  }

  /**
    * Returns connection for given PeerConnectionHandler ActorRef
    *
    * @param handler ActorRef on PeerConnectionHandler actor
    * @return Some(ConnectedPeer) when the connection exists for this handler, and None otherwise
    */
  private def connectionForHandler(handler: ActorRef) = {
    connections.values.find { connectedPeer =>
      connectedPeer.handlerRef == handler
    }
  }

  /**
    * Returns connection for given address of the peer
    *
    * @param peerAddress - socket address of peer
    * @return Some(ConnectedPeer) when the connection exists for this peer, and None otherwise
    */
  private def connectionForPeerAddress(peerAddress: InetSocketAddress) = {
    connections.values.find { connectedPeer =>
      connectedPeer.connectionId.remoteAddress == peerAddress ||
        connectedPeer.peerInfo.exists(peerInfo => getPeerAddress(peerInfo).contains(peerAddress))
    }
  }

  /**
    * Checks the node owns the address
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, bindAddress, scorexContext.externalNodeAddress)
  }

  /**
    * Returns local address of peer for local connections and WAN address of peer for
    * external connections. When local address is not known, try to ask it at the UPnP gateway
    *
    * @param peer - known information about peer
    * @return socket address of the peer
    */
  private def getPeerAddress(peer: PeerInfo): Option[InetSocketAddress] = {
    (peer.peerSpec.localAddressOpt, peer.peerSpec.declaredAddress) match {
      case (Some(localAddr), _) =>
        Some(localAddr)

      case (None, Some(declaredAddress))
        if scorexContext.externalNodeAddress.exists(_.getAddress == declaredAddress.getAddress) =>

        scorexContext.upnpGateway.flatMap(_.getLocalAddressForExternalPort(declaredAddress.getPort))

      case _ => peer.peerSpec.declaredAddress
    }
  }

  /**
    * Returns the node address reachable from Internet
    *
    * @param localSocketAddress - local socket address of the connection to the peer
    * @return - socket address of the node
    */
  private def getNodeAddressForPeer(localSocketAddress: InetSocketAddress) = {
    val localAddr = localSocketAddress.getAddress
    scorexContext.externalNodeAddress match {
      case Some(extAddr) =>
        Some(extAddr)

      case None =>
        if (!localAddr.isSiteLocalAddress && !localAddr.isLoopbackAddress
          && localSocketAddress.getPort == settings.bindAddress.getPort) {
          Some(localSocketAddress)
        } else {
          val listenAddrs = NetworkUtils.getListenAddresses(settings.bindAddress)
            .filterNot(addr => addr.getAddress.isSiteLocalAddress || addr.getAddress.isLoopbackAddress)

          listenAddrs.find(addr => localAddr == addr.getAddress).orElse(listenAddrs.headOption)
        }
    }
  }

  private def validateDeclaredAddress(): Unit = {
    if (!settings.localOnly) {
      settings.declaredAddress.foreach { mySocketAddress =>
        Try {
          val uri = new URI("http://" + mySocketAddress)
          val myHost = uri.getHost
          val myAddress = InetAddress.getAllByName(myHost)

          val listenAddresses = NetworkUtils.getListenAddresses(bindAddress)
          val upnpAddress = scorexContext.upnpGateway.map(_.externalAddress)

          val valid = listenAddresses.exists(myAddress.contains) || upnpAddress.exists(myAddress.contains)

          if (!valid) {
            log.error(
              s"""Declared address validation failed:
                 | $mySocketAddress not match any of the listening address: $listenAddresses
                 | or Gateway WAN address: $upnpAddress""".stripMargin)
          }
        } recover { case t: Throwable =>
          log.error("Declared address validation failed: ", t)
        }
      }
    }
  }

  /**
    * Close connection and temporarily ban peer.
    */
  private def blacklist(peer: ConnectedPeer, penalty: PenaltyType): Unit = {
    connections = connections.filterNot { x => // clear all connections related to banned peer ip
      Option(peer.connectionId.remoteAddress.getAddress).exists(Option(x._1.getAddress).contains(_))
    }
    peerManagerRef ! PeerManager.ReceivableMessages.AddToBlacklist(peer.connectionId.remoteAddress, penalty)
    peer.handlerRef ! CloseConnection
  }

  private def blacklist(peerAddress: InetSocketAddress, penalty: PenaltyType): Unit = {
    connections.get(peerAddress).foreach(blacklist(_, penalty))
  }

}

object NetworkController {

  val ChildActorHandlingRetriesNr: Int = 10

  object ReceivableMessages {

    case class Handshaked(peer: PeerInfo)

    case class RegisterMessageSpecs(specs: Seq[MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case object ShutdownNetwork

    case class ConnectTo(peer: PeerInfo)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class BlacklistPeer(address: InetSocketAddress, penaltyType: PenaltyType)

    case object GetConnectedPeers

  }

}

object NetworkControllerRef {
  def props(settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            tcpManager: ActorRef)(implicit ec: ExecutionContext): Props = {
    Props(new NetworkController(settings, peerManagerRef, scorexContext, tcpManager))
  }

  def apply(settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, scorexContext, IO(Tcp))
    )
  }

  def apply(name: String,
            settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext,
            tcpManager: ActorRef)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, scorexContext, tcpManager),
      name)
  }

  def apply(name: String,
            settings: NetworkSettings,
            peerManagerRef: ActorRef,
            scorexContext: ScorexContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {

    system.actorOf(
      props(settings, peerManagerRef, scorexContext, IO(Tcp)),
      name)
  }
}