package scorex.core.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import scorex.core.network.peer.{LocalAddressPeerFeature, PeerInfo}
import scorex.core.network.peer.PeerManager.ReceivableMessages.{AddOrUpdatePeer, RandomPeers}
import scorex.core.settings.NetworkSettings
import scorex.util.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps


class PeerSynchronizer(val networkControllerRef: ActorRef,
                       peerManager: ActorRef,
                       settings: NetworkSettings,
                       featureSerializers: PeerFeature.Serializers)
                      (implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))
  private val peersSpec = new PeersSpec(featureSerializers)

  override def preStart: Unit = {
    super.preStart()

    networkControllerRef ! RegisterMessageSpecs(Seq(GetPeersSpec, peersSpec), self)

    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkControllerRef ! stn)
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[PeerData]@unchecked, remote)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[PeerData]].isDefined =>

      peers.foreach(peerData => peerManager ! AddOrUpdatePeer(peerData))

    case DataFromPeer(spec, _, peer) if spec.messageCode == GetPeersSpec.messageCode =>

      //todo: externalize the number or increase it (bitcoin uses 1000)
      //todo: check on receiving
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[PeerInfo]]
        .foreach { peers =>
          val msg = Message(peersSpec, Right(peers.map(_.peerData)), None)
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