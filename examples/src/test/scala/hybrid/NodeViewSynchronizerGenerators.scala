package hybrid

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorSystem, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.network.PeerConnectionHandler
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageHandler
import scorex.core.settings.NetworkSettings

// todo: remove unused dependency injections
trait NodeViewSynchronizerGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes =>

  object NodeViewSynchronizerForTests {
    // todo: there is nothing here that is special to tests. Should this `props` method be moved to NodeViewSynchronizer's companion object?
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef,
              syncInfoSpec: SIS,
              networkSettings: NetworkSettings): Props =
      Props(new NodeViewSynchronizer(networkControllerRef,
        viewHolderRef,
        localInterfaceRef,
        syncInfoSpec,
        networkSettings))
  }

  def nodeViewSynchronizer(implicit system: ActorSystem): (ActorRef, PM, ConnectedPeer) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last

    val ncRef: ActorRef = ???
    val vhRef: ActorRef = ???
    val liRef: ActorRef = ???
    val sis: SIS = ???
    val ns: NetworkSettings = ???

    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncRef, vhRef, liRef, sis, ns))
    val m = totallyValidModifier(h, s)
    (ref, m, s, h)
  }
}