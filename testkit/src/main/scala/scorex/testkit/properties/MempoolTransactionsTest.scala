package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}

trait MempoolTransactionsTest[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool]] extends PropSpec
  with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {

  val mempool: MPool
  val transactionGenerator: Gen[TX]

  property("Transactions added to memory pool should be available by id") {
    var m: MPool = mempool
    forAll(transactionGenerator) { tx: TX =>
      m = m.put(tx).get
      m.getById(tx.id).isDefined shouldBe true
    }
  }
}
