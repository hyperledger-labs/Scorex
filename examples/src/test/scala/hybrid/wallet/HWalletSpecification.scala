package hybrid.wallet

import examples.commons.Value
import examples.hybrid.blocks.PosBlock
import examples.hybrid.wallet.HBoxWallet
import hybrid.HybridGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ByteStr
import scorex.crypto.signatures.Signature

import scala.annotation.tailrec
import scala.util.Random

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
class HWalletSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val EmptyBytes = ModifierId @@ Array.fill(32)(0: Byte)
  private val EmptySignature = Signature25519(Signature @@ Array.fill(64)(0: Byte))

  val w: HBoxWallet = HBoxWallet.readOrGenerate(settings.scorexSettings, ByteStr.decodeBase58("p").get).generateNewSecret().generateNewSecret()
  w.secrets.size should be >= 2
  val fs: PrivateKey25519 = w.secrets.head
  val ss: PrivateKey25519 = w.secrets.tail.head

  //todo: what is this test about actually?
  ignore("Wallet should generate same keys") {
    val KeysToGenerate = 5
    @tailrec
    def wallet(oldW: HBoxWallet): HBoxWallet = if (oldW.publicKeys.size >= KeysToGenerate) oldW
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
      whenever(txIn.to.nonEmpty) {
        val toWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Value)] =
          txIn.to.map(p => (ss.publicImage, Value @@ (p._2 + 1)))
        val tx = txIn.copy(to = toWithMyPubkey)

        val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, Array(), EmptySignature)
        val boxes = w.scanPersistent(pb).boxes()
        boxes.exists(b => b.transactionId sameElements tx.id) shouldBe true
      }
    }
  }

  property("Wallet should remove boxes where he is sender") {
    forAll(simpleBoxTransactionGen, noncedBoxGen) { (txIn, box) =>
      val existingBoxes = w.boxes()
      val boxToRemove = existingBoxes(Random.nextInt(existingBoxes.length)).box

      val tx = txIn.copy(from = (boxToRemove.proposition, boxToRemove.nonce) +: txIn.from)
      tx.boxIdsToOpen.exists(id => id sameElements boxToRemove.id) shouldBe true

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, Array(), EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.box.id sameElements boxToRemove.id) shouldBe false
    }
  }


}
