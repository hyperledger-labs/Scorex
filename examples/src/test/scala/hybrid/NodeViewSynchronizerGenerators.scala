package hybrid

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import commons.ExamplesCommonGenerators
import io.iohk.iodb.ByteArrayWrapper
import org.scalacheck.Gen
import scorex.core.VersionTag
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.NetworkSettings
import examples.hybrid.history.HybridSyncInfoMessageSpec
import examples.hybrid.mining.HybridSettings
import scorex.core.transaction.Transaction
import scorex.testkit.generators.CoreGenerators
import scorex.core.consensus.SyncInfo


// todo: remove unused dependency injections
trait NodeViewSynchronizerGenerators {
  this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes with CoreGenerators with ExamplesCommonGenerators =>

  object NodeViewSynchronizerForTests {
    // todo: there is nothing here that is special to tests. Should this `props` method be moved to NodeViewSynchronizer's companion object?
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef,
              syncInfoSpec: SIS): Props =
      Props(new NodeViewSynchronizer[P, TX, HSI, SIS](
        networkControllerRef, viewHolderRef, localInterfaceRef, syncInfoSpec, settings.scorexSettings.network))
  }

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, HSI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val liProbe = TestProbe("LocalInterfaceProbe")
    val sis = HybridSyncInfoMessageSpec

    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)
    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncProbe.ref, vhProbe.ref, liProbe.ref, sis))
    val m = totallyValidModifier(h, s)
    val tx: TX = simpleBoxTransactionGen.sample.get

    val pchProbe = TestProbe("PeerHandlerProbe")

    val address: InetSocketAddress = inetSocketAddressGen.sample.get

    val p : ConnectedPeer = ConnectedPeer(address, pchProbe.ref)

    (ref, h.syncInfo(false), m, tx, p, pchProbe, ncProbe, vhProbe, liProbe)
  }
}