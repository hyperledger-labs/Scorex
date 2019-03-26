package scorex.core.network

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.message.{GetPeersSpec, Message, PeersSpec}
import scorex.core.network.peer.PeerInfo
import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(val networkControllerRef: ActorRef,
                       peerManager: ActorRef,
                       settings: NetworkSettings,
                       featureSerializers: PeerFeature.Serializers)
                      (implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))
  private val peersSpec = new PeersSpec(featureSerializers, settings.maxPeerSpecObjects)

  override def preStart: Unit = {
    super.preStart()

    networkControllerRef ! RegisterMessageSpecs(Seq(GetPeersSpec, peersSpec), self)

    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, settings.getPeersInterval)(networkControllerRef ! stn)
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[PeerSpec]@unchecked, _)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[PeerSpec]].isDefined =>

      peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))

    case DataFromPeer(spec, _, peer) if spec.messageCode == GetPeersSpec.messageCode =>

      (peerManager ? RecentlySeenPeers(settings.maxPeerSpecObjects))
        .mapTo[Seq[PeerInfo]]
        .foreach { peers =>
          val msg = Message(peersSpec, Right(peers.map(_.peerSpec)), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeers(Seq(peer)))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}

object PeerSynchronizerRef {
  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings,
            featureSerializers: PeerFeature.Serializers)(implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings, featureSerializers))

  def apply(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings,
            featureSerializers: PeerFeature.Serializers)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings, featureSerializers))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings,
            featureSerializers: PeerFeature.Serializers)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings, featureSerializers), name)
}