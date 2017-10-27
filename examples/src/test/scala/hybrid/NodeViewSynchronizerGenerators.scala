package hybrid

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.network.PeerConnectionHandler
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageHandler
import scorex.core.settings.NetworkSettings
import scorex.core.utils.ScorexLogging

import examples.hybrid.history.HybridSyncInfoMessageSpec

// todo: remove unused dependency injections
trait NodeViewSynchronizerGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes =>

  object NodeViewSynchronizerForTests {
    // todo: there is nothing here that is special to tests. Should this `props` method be moved to NodeViewSynchronizer's companion object?
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef,
              syncInfoSpec: SIS,
              networkSettings: NetworkSettings): Props =
      Props(new NodeViewSynchronizer[P, TX, SI, SIS](networkControllerRef, // todo: do the type parameters really need to be passed explicitly here?
        viewHolderRef,
        localInterfaceRef,
        syncInfoSpec,
        networkSettings))
  }


  // todo: move this to a PeerConnectionHandlerGenerators?
  object PeerConnectionHandlerForTests {
    def props(settings: NetworkSettings,
              networkControllerRef: ActorRef,
              peerManager: ActorRef,
              messagesHandler: MessageHandler,
              connection: ActorRef,
              ownSocketAddress: Option[InetSocketAddress],
              remote: InetSocketAddress): Props =
      Props(new PeerConnectionHandler(
        settings, networkControllerRef, peerManager, messagesHandler, connection, ownSocketAddress, remote
      ))
  }

  class DummyActor extends Actor with ScorexLogging {
    override def receive: Receive = {
      case m => log.info(m.toString)
    }
  }
  object DummyActor {
    def props(): Props = Props(new DummyActor)
  }


  // fixme: the following lines were copy-pasted from "src/test/scala/scorex/ObjectGenerators.scala" because it cannot be imported here.
  private val MaxIp = 255
  private val MaxPort = 65535
  private lazy val inetSocketAddressGen = for {
    ip1 <- Gen.choose(0, MaxIp)
    ip2 <- Gen.choose(0, MaxIp)
    ip3 <- Gen.choose(0, MaxIp)
    ip4 <- Gen.choose(0, MaxIp)
    port <- Gen.choose(0, MaxPort)
  } yield new InetSocketAddress(InetAddress.getByName(s"$ip1.$ip2.$ip3.$ip4"), port)


  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last

    val ncRef: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val vhRef: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val liRef: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val sis = HybridSyncInfoMessageSpec
    val ns: NetworkSettings = ???

    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncRef, vhRef, liRef, sis, ns))
    val m = totallyValidModifier(h, s)



    val networkControllerRef: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val peerManagerRef: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val messageHandler: MessageHandler = system.actorOf(DummyActor.props()) // todo: is this ok?
    val connection: ActorRef = system.actorOf(DummyActor.props()) // todo: is this ok?
    val ownSocketAddress: Option[InetSocketAddress] = None // todo: does this make sense?
    val remote: InetSocketAddress = inetSocketAddressGen.sample.get // todo: is this ok?

    val pchRef: ActorRef = system.actorOf(PeerConnectionHandlerForTests.props(ns, ncRef, peerManagerRef, messageHandler, connection, ownSocketAddress, remote))
    val address: InetSocketAddress = inetSocketAddressGen.sample.get // todo: is this ok?

    val p : ConnectedPeer = ConnectedPeer(address, pchRef)

    (ref, m, p)
  }
}