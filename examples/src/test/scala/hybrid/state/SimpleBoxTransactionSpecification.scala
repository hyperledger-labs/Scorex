package hybrid.state

import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.proof.Signature25519

class SimpleBoxTransactionSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  property("Generated transaction is valid") {
    forAll(simpleBoxTransactionGen) { tx =>
      tx.isValid shouldBe true
    }
  }

  property("Transaction with modified signature is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val wrongSig: Array[Byte] = (tx.signatures.head.bytes.head + 1).toByte +: tx.signatures.head.bytes.tail
      val wrongSigs = (Signature25519(wrongSig) +: tx.signatures.tail).toIndexedSeq
      tx.copy(signatures = wrongSigs).isValid shouldBe false
    }
  }

  property("Transaction with modified from is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      val wrongFromPub = tx.from.map(p => (p._1, p._2 + 1))
      tx.copy(from = wrongFromPub).isValid shouldBe false
    }
  }

  property("Transaction with modified timestamp is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      tx.copy(timestamp = tx.timestamp + 1).isValid shouldBe false
    }
  }

  property("Transaction with modified fee is invalid") {
    forAll(simpleBoxTransactionGen) { tx =>
      tx.copy(fee = tx.fee + 1).isValid shouldBe false
    }
  }

}
