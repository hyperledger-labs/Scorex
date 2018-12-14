package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import commons.ExamplesCommonGenerators
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.HybridApp
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoMessageSpec}
import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedMempool}
import scorex.core.network._
import scorex.core.transaction.ReferenceMempoolActor
import scorex.core.utils.NetworkTimeProvider
import scorex.testkit.generators.CoreGenerators
import scorex.testkit.properties.SynchronizerFixture
import scorex.testkit.utils.SysId

import scala.concurrent.ExecutionContext.Implicits.global

trait NodeViewSynchronizerGenerators {
  this: ModifierGenerators
    with StateGenerators
    with HistoryGenerators
    with HybridTypes
    with CoreGenerators
    with ExamplesCommonGenerators =>

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

  def createFixture(): SynchronizerFixture[SimpleBoxTransaction, HybridBlock, HybridSyncInfo] = {
    implicit val system: ActorSystem = ActorSystem("WithIsoFix-%d".format(SysId.incrementAndGet()))
    val h = historyGen.sample.getOrElse(throw new Exception("Empty historyGen.sample"))
    val sRaw = stateGen.sample.getOrElse(throw new Exception("Empty stateGen.sample"))
    val mempool = SimpleBoxTransactionMemPool.emptyPool
    val v = h.openSurfaceIds().lastOption.getOrElse(throw new Exception("Empty history.openSurfaceIds"))
    sRaw.store.update(ByteArrayWrapper(idToBytes(v)), Seq(), Seq())
    val s = sRaw.copy(version = idToVersion(v))

    val ncProbe = TestProbe("NetworkControllerProbe")
    val vhProbe = TestProbe("ViewHolderProbe")
    val pchProbe = TestProbe("PeerHandlerProbe")

    val syncRef = system.actorOf(NodeViewSynchronizerForTests.props(ncProbe.ref, vhProbe.ref))
    syncRef ! ChangedHistory(h)
    syncRef ! ChangedMempool(mempool)

    new SynchronizerFixture(
      system,
      node = syncRef,
      memoryPool = ReferenceMempoolActor[TX, SimpleBoxTransactionMemPool](mempool),
      syncInfo = h.syncInfo,
      mod = totallyValidModifier(h, s),
      tx = simpleBoxTransactionGen.sample.getOrElse(throw new Exception("Empty simpleBoxTransactionGen.sample")),
      peer = connectedPeerGen(pchProbe.ref).sample.getOrElse(throw new Exception("Empty connectedPeerGen.sample")),
      pchProbe,
      ncProbe,
      vhProbe,
      eventListener = TestProbe("EventListener")
    )
  }
}
