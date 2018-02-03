package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import commons.ExamplesCommonGenerators
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.VersionTag
import scorex.core.network._
import examples.hybrid.history.HybridSyncInfoMessageSpec
import scorex.core.app.Version
import scorex.core.utils.NetworkTimeProvider
import scorex.testkit.generators.CoreGenerators


trait NodeViewSynchronizerGenerators {
  this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes with CoreGenerators with ExamplesCommonGenerators =>

  object NodeViewSynchronizerForTests {
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef,
              localInterfaceRef: ActorRef): Props =
      NodeViewSynchronizerRef.props[P, TX, HSI, SIS, PM, HT, MP](
        networkControllerRef, viewHolderRef, localInterfaceRef, HybridSyncInfoMessageSpec, settings.scorexSettings.network, new NetworkTimeProvider(settings.scorexSettings.ntp))
  }

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, HSI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe) = {
    val h = historyGen.sample.get
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last
    sRaw.store.update(ByteArrayWrapper(v), Seq(), Seq())
    val s = sRaw.copy(version = VersionTag @@ v)

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val liProbe = TestProbe("LocalInterfaceProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")

    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncProbe.ref, vhProbe.ref, liProbe.ref))
    val m = totallyValidModifier(h, s)
    val tx = simpleBoxTransactionGen.sample.get
    val p : ConnectedPeer = ConnectedPeer(inetSocketAddressGen.sample.get, pchProbe.ref, Outgoing,
      Handshake("", Version(0,1,2), "", None, 0L))

    (ref, h.syncInfo, m, tx, p, pchProbe, ncProbe, vhProbe, liProbe)
  }
}