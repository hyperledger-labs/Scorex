package hybrid.wallet

import examples.hybrid.blocks.PosBlock
import examples.hybrid.mining.MiningSettings
import examples.hybrid.wallet.HWallet
import hybrid.HybridGenerators
import io.circe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

import scala.annotation.tailrec
import scala.util.Random

class HWalletSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val EmptyBytes = Array.fill(32)(0: Byte)
  val EmptySignature = Signature25519(Array.fill(64)(0: Byte))

  val w = HWallet.readOrGenerate(settings, "p").generateNewSecret().generateNewSecret()
  w.secrets.size should be >= 2
  val fs = w.secrets.head
  val ss = w.secrets.tail.head

  property("Wallet should generate same keys") {
    val KeysToGenerate = 5
    @tailrec
    def wallet(oldW: HWallet): HWallet = if (oldW.publicKeys.size >= KeysToGenerate) oldW
    else wallet(oldW.generateNewSecret())

    val keys = wallet(w).publicKeys
    keys.size shouldBe KeysToGenerate
    keys.map(_.toString).mkString(",") shouldBe "4TBtyQqaKmJLL2UqgrVSFt2JNkevZWfwDahPkWM424aeX7ttzc,3TDp4RdDs9HMjmiso4r7jyhyLmECkVwqXXy2V28yLJPdWA4bcq,3qkG6U4v4Vqb85yXLa7wKUDnk7a9iA8zQiampc4Q47yARQG1sY,4qwyT3s6bU21N4dhbmaZSYhFEcDEgLoHJjzfDTfhuktbbNC4da,4nkadHvLyi3ZobRzEamrFkZbNzQAkNpLXUKJEixk6SN6kNQzHa"
  }

  property("Wallet should generate new pairs") {
    val s = w.secrets.size

    val w2 = w.generateNewSecret().generateNewSecret()
    w2.secrets.size shouldBe s + 2
    w.publicKeys.size shouldBe w.secrets.size
  }

  property("Wallet should add boxes where he is recipient") {
    forAll(simpleBoxTransactionGen, noncedBoxGen) { (txIn, box) =>
      val toWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Long)] =
        txIn.to.map(p => (ss.publicImage, p._2 + 1))
      val tx = txIn.copy(to = toWithMyPubkey)

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, Array(), EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.transactionId sameElements tx.id) shouldBe true
    }
  }

  property("Wallet should remove boxes where he is sender") {
    forAll(simpleBoxTransactionGen, noncedBoxGen) { (txIn, box) =>
      val existingBoxes = w.boxes()
      val boxToRemove = existingBoxes(Random.nextInt(existingBoxes.length)).box

      val fromWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Long)] =
        txIn.from.map(p => (boxToRemove.proposition, boxToRemove.nonce))
      val tx = txIn.copy(from = fromWithMyPubkey)
      tx.boxIdsToOpen.exists(id => id sameElements boxToRemove.id) shouldBe true

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, Array(), EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.box.id sameElements boxToRemove.id) shouldBe false
    }
  }


}
