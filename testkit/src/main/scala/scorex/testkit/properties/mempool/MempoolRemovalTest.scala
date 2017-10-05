package scorex.testkit.properties.mempool

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.ArbitraryTransactionsCarryingModifierProducer

trait MempoolRemovalTest[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool],
PM <: PersistentNodeViewModifier,
CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[P, TX],
HT <: History[PM, SI, HT],
SI <: SyncInfo] extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with PropertyChecks
  with ScorexLogging
  with TestkitHelpers
  with MemoryPoolTest[P, TX, MPool]
  with ArbitraryTransactionsCarryingModifierProducer[P, TX, MPool, PM, CTM] {

  val historyGen: Gen[HT]

  //todo: this test doesn't check anything. It should be reworked as a test for node view holder
  property("Transactions once added to block should be removed from Mempool") {
    forAll(Gen.choose(1, 10)) { noOfTransactionsFromMempool: Int =>
      var m: MPool = memPool
      var h: HT = historyGen.sample.get
      forAll(transactionGenerator) { tx: TX =>
        m = m.put(tx).get
      }
      var prevMempoolSize = m.size
      val b = modifierWithTransactions(Some(m), None)
    //todo: fix    (m.size + b.transactions.get.size) shouldEqual prevMempoolSize
    }
  }
}


