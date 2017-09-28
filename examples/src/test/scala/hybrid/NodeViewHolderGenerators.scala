package hybrid

import akka.actor.{ActorRef, ActorSystem, Props}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.HybridNodeViewHolder
import examples.hybrid.blocks._
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import scorex.core.consensus.SyncInfo
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait NodeViewHolderGenerators { this: ModifierGenerators with StateGenerators with HistoryGenerators with Settings =>

  type P = PublicKey25519Proposition
  type TX = SimpleBoxTransaction
  type PM = HybridBlock
  type SI = SyncInfo

  type NODE = HybridNodeViewHolder
  type ST = HBoxStoredState
  type HT = HybridHistory

  class NodeViewHolderForTests(h: HT, s: ST) extends HybridNodeViewHolder(settings) {

    override protected def genesisState: (HIS, MS, VL, MP) = {
      val gw = HWallet.genesisWallet(settings, Seq.empty)
      (h, s, gw, SimpleBoxTransactionMemPool.emptyPool)
    }

    override def restoreState(): Option[(HIS, MS, VL, MP)] =
      if (HWallet.exists(settings)) {
        Some((h, s, HWallet.genesisWallet(settings, Seq.empty), SimpleBoxTransactionMemPool.emptyPool))
      } else None

  }

  object NodeViewHolderForTests {
      def props(h: HT, s: ST): Props = Props(new NodeViewHolderForTests(h, s))
  }

  def nodeViewHolder(implicit system: ActorSystem): (ActorRef, PM, ST, HT) = {
    val h = historyGen.sample.get
    val s = stateGen.sample.get
    val ref = system.actorOf(NodeViewHolderForTests.props(h, s))
    val m = totallyValidModifier(h, s)
    (ref, m, s, h)
  }
}
