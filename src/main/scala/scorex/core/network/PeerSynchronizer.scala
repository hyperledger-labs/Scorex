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

class PeerSynchronizer(val networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
                      (implicit ec: ExecutionContext) extends Actor with ScorexLogging {

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))

  override def preStart: Unit = {
    super.preStart()

    networkControllerRef ! RegisterMessageSpecs(Seq(GetPeersSpec, PeersSpec), self)

    val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
    val stn = SendToNetwork(msg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkControllerRef ! stn)
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[InetSocketAddress]@unchecked, _)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>

      peers.foreach(isa => peerManager ! AddOrUpdatePeer(isa, None, None, Seq()))

    case DataFromPeer(spec, _, peer) if spec.messageCode == GetPeersSpec.messageCode =>

      //todo: externalize the number, check on receiving
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[PeerInfo]]
        .foreach { peers =>
          val addresses = if (peer.remoteAddress.getAddress.isSiteLocalAddress) {
            peers.flatMap { peer =>
              peer.features
                .collectFirst { case LocalAddressPeerFeature(addr) => addr }
                .orElse(peer.declaredAddress)
            }
          } else {
            peers.flatMap(_.declaredAddress)
          }

          val msg = Message(PeersSpec, Right(addresses), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeers(Seq(peer)))
        }

    case nonsense =>
      log.warn(s"Unhandled message: $nonsense")
  }

}

object PeerSynchronizerRef {

  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings))

  def apply(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings), name)
}
