package scorex.testkit.properties.mempool

import java.security.MessageDigest

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.Transaction
import scorex.core.utils._

trait MempoolFilterPerformanceTest[TX <: Transaction]
  extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with PropertyChecks
    with MemoryPoolTest[TX] {

  val thresholdInHashes = 500000

  private val HeatJVMHashesCount = 1000000 //to heat up JVM, just in case it is cold

  val thresholdSecs: Double = {
    //heat up
    (1 to HeatJVMHashesCount).foreach(i => MessageDigest.getInstance("SHA-256").digest(("dummy" + i).getBytes()))

    val t0 = System.currentTimeMillis()
    (1 to thresholdInHashes).foreach(i => MessageDigest.getInstance("SHA-256").digest(("dummy" + i).getBytes()))
    val t = System.currentTimeMillis()
    (t - t0) / 1000.0
  }

  val m = defaultMempool

  property("Mempool should be able to store a lot of transactions") {
    (0 until 1000) foreach { _ =>
      forAll(transactionGenerator) { tx: TX =>
        m.put(tx)
      }
    }
    m.size should be > 1000
  }

  property("Mempool filter of non-existing transaction should be fast") {
    forAll(transactionGenerator) { tx: TX =>
      val (time, _) = profile(m.remove(tx))
      assert(time < thresholdSecs)
    }
  }

  property("Mempool filter of existing transaction should be fast") {
    forAll(transactionGenerator) { tx: TX =>
      m.put(tx).get
      val (time, _) = profile(m.remove(tx))
      assert(time < thresholdSecs)
    }
  }
}
