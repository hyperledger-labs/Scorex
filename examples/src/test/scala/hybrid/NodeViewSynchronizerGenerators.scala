package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import commons.ExamplesCommonGenerators
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.HybridApp
import examples.hybrid.blocks.{PosBlock, PosBlockSerializer, PowBlock, PowBlockSerializer}
import examples.hybrid.history.HybridSyncInfoMessageSpec
import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import scorex.core.network._
import scorex.util.serialization.{Reader, Writer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.NetworkTimeProvider
import scorex.testkit.generators.CoreGenerators

import scala.concurrent.ExecutionContext.Implicits.global

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
trait NodeViewSynchronizerGenerators {
  this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes with CoreGenerators with ExamplesCommonGenerators =>

  object NodeViewSynchronizerForTests {
    def props(networkControllerRef: ActorRef,
              viewHolderRef: ActorRef): Props =
      NodeViewSynchronizerRef.props[TX, HSI, SIS, PM, HT, MP](networkControllerRef,
        viewHolderRef,
        HybridSyncInfoMessageSpec,
        settings.scorexSettings.network,
        new NetworkTimeProvider(settings.scorexSettings.ntp),
        HybridApp.modifierSerializers)
  }

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, HSI, PM, TX, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, ScorexSerializer[PM]) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val sRaw = stateGen.sample.get
    val mempool = SimpleBoxTransactionMemPool.emptyPool
    val v = h.openSurfaceIds().last
    sRaw.store.update(ByteArrayWrapper(idToBytes(v)), Seq(), Seq())
    val s = sRaw.copy(version = idToVersion(v))

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")
    val eventListener = TestProbe("EventListener")

    val ref = system.actorOf(NodeViewSynchronizerForTests.props(ncProbe.ref, vhProbe.ref))
    ref ! ChangedHistory(h)
    ref ! ChangedMempool(mempool)
    val m = totallyValidModifier(h, s)
    val modSerializer = new ScorexSerializer[PM] {
      override def serialize(obj: PM, w: Writer): Unit = {
        obj match {
          case block: PowBlock => PowBlockSerializer.serialize(block, w)
          case block: PosBlock => PosBlockSerializer.serialize(block, w)
        }
      }

      override def parse(r: Reader): PM = {
        m match {
          case block: PowBlock => PowBlockSerializer.parse(r)
          case block: PosBlock => PosBlockSerializer.parse(r)
        }
      }
    }

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val tx = simpleBoxTransactionGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val p: ConnectedPeer = connectedPeerGen(pchProbe.ref).sample.get

    (ref, h.syncInfo, m, tx, p, pchProbe, ncProbe, vhProbe, eventListener, modSerializer)
  }
}