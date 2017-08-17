package scorex.testkit.properties.mempool

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils.ScorexLogging
import scorex.testkit.TestkitHelpers

trait MempoolRemovalTest[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool],
PM <: PersistentNodeViewModifier[P, TX],
HT <: History[P, TX, PM, SI, HT],
SI <: SyncInfo] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks
  with ScorexLogging with TestkitHelpers {

  val mempool: MPool
  val transactionGenerator: Gen[TX]
  val history: HT

  def genValidModifier(history: HT, mempoolTransactionFetchOption: Boolean, noOfTransactionsFromMempool: Int): PM

  property("Transactions once added to block should be removed from Mempool") {
    forAll(Gen.choose(1, 10)) { noOfTransactionsFromMempool: Int =>
      var m: MPool = mempool
      var h: HT = history
      forAll(transactionGenerator) { tx: TX =>
        m = m.put(tx).get
      }
      var prevMempoolSize = m.size
      val b = genValidModifier(h, mempoolTransactionFetchOption = true, noOfTransactionsFromMempool)
      (m.size + b.transactions.get.size) shouldEqual prevMempoolSize
    }
  }
}


