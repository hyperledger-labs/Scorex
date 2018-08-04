package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import examples.commons.SimpleBoxTransactionMemPool
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.wallet.HBoxWallet
import io.iohk.iodb.ByteArrayWrapper
import scorex.core._
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
trait NodeViewHolderGenerators {
  this: ModifierGenerators with StateGenerators with HistoryGenerators with HybridTypes =>

  class NodeViewHolderForTests(h: HT, s: ST) extends HybridNodeViewHolder(settings, new NetworkTimeProvider(settings.scorexSettings.ntp)) {

    override protected def genesisState: (HIS, MS, VL, MP) = {
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val store = lsmStoreGen.sample.get
      val seed = Array.fill(10)(1: Byte)
      val gw = new HBoxWallet(seed, store)
      (h, s, gw, SimpleBoxTransactionMemPool.emptyPool)
    }

    override def restoreState(): Option[(HIS, MS, VL, MP)] = None
  }

  object NodeViewHolderForTests {
    def props(h: HT, s: ST): Props = Props(new NodeViewHolderForTests(h, s))
  }

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, TestProbe, PM, ST, HT) = {
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val h = historyGen.sample.get
    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val sRaw = stateGen.sample.get
    val v = h.openSurfaceIds().last
    sRaw.store.update(ByteArrayWrapper(idToBytes(v)), Seq(), Seq())
    val s = sRaw.copy(version = idToVersion(v))
    val ref = system.actorOf(NodeViewHolderForTests.props(h, s))
    val m = totallyValidModifier(h, s)
    val eventListener = TestProbe()
    (ref, eventListener, m, s, h)
  }
}