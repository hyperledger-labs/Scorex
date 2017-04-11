package scorex.testkit.properties

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.utils._

trait MempoolFilterPerformanceTest[P <: Proposition,
TX <: Transaction[P],
MPool <: MemoryPool[TX, MPool]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks {

  val mempool: MPool
  val transactionGenerator: Gen[TX]
  var initizlizedMempool: Option[MPool] = None

  property("Mempool should be able to store a lot of transactions") {
    var m: MPool = mempool
    (0 until 1000) foreach { _ =>
      forAll(transactionGenerator) { tx: TX =>
        m = m.put(tx).get
      }
    }
    m.size should be > 1000
    initizlizedMempool = Some(m)
  }

  property("Mempool filter of non-existing transaction should be fast") {
    val m = initizlizedMempool.get
    forAll(transactionGenerator) { tx: TX =>
      val (time, _) = profile(m.filter(Seq(tx)))
      assert(time < 0.001)
    }
  }

  property("Mempool filter of existing transaction should be fast") {
    var m = initizlizedMempool.get
    forAll(transactionGenerator) { tx: TX =>
      m = m.put(tx).get
      val (time, _) = profile(m.filter(Seq(tx)))
      assert(time < 0.001)
    }
  }


}
