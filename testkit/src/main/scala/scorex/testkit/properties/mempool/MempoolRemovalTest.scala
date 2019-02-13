package scorex.testkit.properties.mempool

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.{PersistentNodeViewModifier, TransactionsCarryingPersistentNodeViewModifier}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.Transaction
import scorex.testkit.TestkitHelpers
import scorex.testkit.generators.ArbitraryTransactionsCarryingModifierProducer
import scorex.util.ScorexLogging

trait MempoolRemovalTest[TX <: Transaction,
    PM <: PersistentNodeViewModifier,
    CTM <: PM with TransactionsCarryingPersistentNodeViewModifier[TX],
    HT <: History[PM, SI, HT],
    SI <: SyncInfo]
  extends PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with PropertyChecks
  with ScorexLogging
  with TestkitHelpers
  with MemoryPoolTest[TX]
  with ArbitraryTransactionsCarryingModifierProducer[TX, PM, CTM] {

  val historyGen: Gen[HT]

  //todo: this test doesn't check anything. It should be reworked as a test for node view holder
  ignore("Transactions once added to block should be removed from Mempool") {
    val min = 1
    val max = 10
    forAll(Gen.choose(min, max)) { noOfTransactionsFromMempool: Int =>
      forAll(transactionGenerator) { tx: TX =>
        //mempool.put(tx)
      }
      // var prevMempoolSize = m.size
      //val b = modifierWithTransactions(Some(mempool.getReader), None)
      //todo: fix    (m.size + b.transactions.get.size) shouldEqual prevMempoolSize
    }
  }
}


