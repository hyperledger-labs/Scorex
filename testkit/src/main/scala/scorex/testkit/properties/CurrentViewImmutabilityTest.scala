package scorex.testkit.properties

import scorex.core.NodeViewHolder.CurrentView
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.MinimalState
import scorex.core.transaction.wallet.Wallet
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.testkit.TestkitHelpers

trait CurrentViewImmutabilityTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT],
MPool <: MemoryPool[TX, MPool]] extends StateTests[P, TX, PM, B, ST] with TestkitHelpers {

  val history: HT
  val mempool: MPool
  val wallet: Wallet[P, TX, PM, _]
  val state: ST

  def genValidModifier(history: HT): PM

  property("CurrentView should be immutable") {
    check { _ =>
      val h = history
      val currentView = CurrentView[HT, ST, Wallet[P, TX, PM, _], MPool](history, state, wallet, mempool)
      val b = genValidModifier(h)

      currentView.history.modifierById(b.id).isDefined shouldBe false
      h.append(b)
      //changing h should not affect currentView.history
      currentView.history.modifierById(b.id).isDefined shouldBe false
    }
  }

}
