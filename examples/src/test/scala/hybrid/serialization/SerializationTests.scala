package hybrid.serialization

import examples.commons.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer, SimpleBoxTransaction, SimpleBoxTransactionSerializer}
import examples.hybrid.blocks.{PosBlock, PosBlockSerializer, PowBlock, PowBlockSerializer}
import examples.hybrid.history.{HybridSyncInfo, HybridSyncInfoSerializer}
import hybrid.HybridGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.wallet.{WalletBox, WalletBoxSerializer}

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
class SerializationTests extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators {

  property("WalletBox serialization") {
    val walletBoxSerializer =
      new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox](PublicKey25519NoncedBoxSerializer)
    forAll(walletBoxGen) { b: WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox] =>
      val parsed = walletBoxSerializer.parseBytes(walletBoxSerializer.toBytes(b))
      walletBoxSerializer.toBytes(parsed) shouldEqual walletBoxSerializer.toBytes(b)
    }
  }

  property("PublicKey25519NoncedBox serialization") {
    forAll(noncedBoxGen) { b: PublicKey25519NoncedBox =>
      val parsed = PublicKey25519NoncedBoxSerializer.parseBytes(PublicKey25519NoncedBoxSerializer.toBytes(b))
      parsed shouldEqual b
      PublicKey25519NoncedBoxSerializer.toBytes(parsed) shouldEqual PublicKey25519NoncedBoxSerializer.toBytes(b)
    }
  }

  property("PosBlock serialization") {
    forAll(posBlockGen) { b: PosBlock =>
      val parsed = PosBlockSerializer.parseBytes(PosBlockSerializer.toBytes(b))
      PosBlockSerializer.toBytes(parsed) shouldEqual PosBlockSerializer.toBytes(b)
    }
  }

  property("PowBlock serialization") {
    forAll(powBlockGen) { b: PowBlock =>
      val parsed = PowBlockSerializer.parseBytes(PowBlockSerializer.toBytes(b))
      parsed.brothersCount shouldBe b.brothersCount
      parsed.brothersHash shouldEqual b.brothersHash
      parsed.brothers.headOption.forall(ph => java.util.Arrays.equals(ph.brothersHash, b.brothers.head.brothersHash)) shouldBe true
      PowBlockSerializer.toBytes(parsed) shouldEqual PowBlockSerializer.toBytes(b)
    }
  }

  property("SimpleBoxTransaction serialization") {
    forAll(simpleBoxTransactionGen) { b: SimpleBoxTransaction =>
      val parsed = SimpleBoxTransactionSerializer.parseBytes(SimpleBoxTransactionSerializer.toBytes(b))
      SimpleBoxTransactionSerializer.toBytes(parsed) shouldEqual SimpleBoxTransactionSerializer.toBytes(b)
    }
  }

  property("HybridSyncInfo serialization") {
    forAll(hybridSyncInfoGen) { b: HybridSyncInfo =>
      val parsed = HybridSyncInfoSerializer.parseByteString(HybridSyncInfoSerializer.toByteString(b))
      HybridSyncInfoSerializer.toByteString(parsed) shouldEqual HybridSyncInfoSerializer.toByteString(b)
    }
  }
}
