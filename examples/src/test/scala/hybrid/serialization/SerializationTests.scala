package hybrid.serialization

import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{PosBlock, PowBlock}
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoSerializer}
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.{WalletBox, WalletBoxSerializer}

class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  property("WalletBox serialization") {
    val walletBoxSerializer =
      new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox](PublicKey25519NoncedBoxSerializer)
    forAll(walletBoxGen) { b: WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox] =>
      val parsed = walletBoxSerializer.parseBytes(walletBoxSerializer.toBytes(b)).get
      walletBoxSerializer.toBytes(parsed) shouldEqual walletBoxSerializer.toBytes(b)
    }
  }

  property("PublicKey25519NoncedBox serialization") {
    forAll(noncedBoxGen) { b: PublicKey25519NoncedBox =>
      val parsed = PublicKey25519NoncedBoxSerializer.parseBytes(PublicKey25519NoncedBoxSerializer.toBytes(b)).get
      parsed shouldEqual b
      PublicKey25519NoncedBoxSerializer.toBytes(parsed) shouldEqual PublicKey25519NoncedBoxSerializer.toBytes(b)
    }
  }

  property("PosBlock serialization") {
    forAll(posBlockGen) { b: PosBlock =>
      val parsed = b.serializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("PowBlock serialization") {
    forAll(powBlockGen) { b: PowBlock =>
      val parsed = b.serializer.parseBytes(b.bytes).get
      parsed.brothersCount shouldBe b.brothersCount
      parsed.brothersHash shouldEqual b.brothersHash
      parsed.brothers.headOption.forall(ph => ph.brothersHash sameElements b.brothers.head.brothersHash) shouldBe true
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("SimpleBoxTransaction serialization") {
    forAll(simpleBoxTransactionGen) { b: SimpleBoxTransaction =>
      val parsed = b.serializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

  property("HybridSyncInfo serialization") {
    forAll(hybridSyncInfoGen) { b: HybridSyncInfo =>
      val parsed = HybridSyncInfoSerializer.parseBytes(b.bytes).get
      parsed.bytes shouldEqual b.bytes
    }
  }

}
