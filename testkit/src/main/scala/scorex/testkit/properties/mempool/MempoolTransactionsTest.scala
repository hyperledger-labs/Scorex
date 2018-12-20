package scorex.testkit.properties.mempool

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.Transaction

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
trait MempoolTransactionsTest[TX <: Transaction]
  extends PropSpec
    with GeneratorDrivenPropertyChecks
    with Matchers
    with PropertyChecks
    with MemoryPoolTest[TX] {
  
  val transactionSeqGenerator: Gen[Seq[TX]] = Gen.nonEmptyContainerOf[Seq, TX](transactionGenerator)

  property("Size of mempool should increase when adding a non-present transaction") {
    forAll(mempoolGenerator, transactionGenerator) { (m: MPool, tx: TX) =>
      m.put(tx)
      m.size shouldEqual 1
    }
  }

  property("Size of mempool should not increase when adding a present transaction") {
    forAll(mempoolGenerator, transactionGenerator) { (m: MPool, tx: TX) =>
      m.put(tx)
      m.put(tx)
      m.size shouldEqual 1
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (with check)") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.put)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present txs with duplicates (with check)") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      (txs ++ txs).foreach(m.put)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (without check)") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.putWithoutCheck)
      m.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present transactions with duplicates (without check)") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      (txs ++ txs).foreach(m.putWithoutCheck)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should decrease when removing a present transaction") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.put)
      m.remove(txs.headOption.get)
      m.size shouldBe txs.size - 1
    }
  }

  property("Size of mempool should not decrease when removing a non-present transaction") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      txs.foreach(m.put)
      m.remove(tx)
      m.size shouldBe txs.size
    }
  }

  property("Mempool transactions should be filtered successfully") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.put)
      m.filterBy(tx => tx equals txs.headOption.get)
      m.size shouldBe 1
    }
  }

  property("Present transactions should be available by id") {
    forAll(mempoolGenerator, transactionGenerator) { (m: MPool, tx: TX) =>
      m.put(tx)
      m.modifierById(tx.id).isDefined shouldBe true
    }
  }

  property("Non-present transactions should not be available by id") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      txs.foreach(m.put)
      m.modifierById(tx.id).isDefined shouldBe false
    }
  }

  property("Mempool should contain present transactions") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.put)
      m.contains(txs.headOption.get.id) shouldBe true
    }
  }

  property("Mempool should not contain non-present transactions") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      txs.foreach(m.put)
      m.contains(tx.id) shouldBe false
    }
  }

  property("Present transactions should be obtained by their ids") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      (txs :+ tx).foreach(m.put)
      m.getAll(txs.map(_.id)) sameElements txs
    }
  }

  property("Non-present transactions should not be obtained by their ids") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      m.put(tx)
      m.getAll(txs.map(_.id)).size shouldBe 0
    }
  }

  property("Required number of transactions should be taken from mempool") {
    forAll(mempoolGenerator, transactionSeqGenerator, transactionGenerator) { (m: MPool, txs: Seq[TX], tx: TX) =>
      (txs :+ tx).foreach(m.put)
      m.take(txs.size).size shouldBe txs.size
    }
  }

  property("Maximum number of transactions that can be taken should equals mempool size") {
    forAll(mempoolGenerator, transactionSeqGenerator) { (m: MPool, txs: Seq[TX]) =>
      txs.foreach(m.put)
      m.take(txs.size + 1).size shouldBe m.size
    }
  }
}
