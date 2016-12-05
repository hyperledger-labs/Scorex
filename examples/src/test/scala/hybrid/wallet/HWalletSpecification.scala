package hybrid.wallet

import examples.hybrid.blocks.PosBlock
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.wallet.HWallet
import hybrid.HybridGenerators
import io.circe
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

class HWalletSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }
  val EmptyBytes = Array.fill(32)(0: Byte)
  val EmptySignature = Signature25519(Array.fill(64)(0: Byte))

  val w = HWallet.readOrGenerate(settings, "p").generateNewSecret().generateNewSecret()
  w.secrets.size should be >= 2
  val fs = w.secrets.head
  val ss = w.secrets.tail.head

  property("Wallet should generate new pairs") {
    val s = w.secrets.size

    val w2 = w.generateNewSecret().generateNewSecret()
    w2.secrets.size shouldBe s + 2
  }

  property("Wallet should scan persistent with sender from wallet") {
    forAll(simpleBoxTransactionGen) { txIn =>
      val fromWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Long)] =
        txIn.from.map(p => (ss.publicImage, p._2 + 1))
      val tx = txIn.copy(from = fromWithMyPubkey)

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), fs.publicImage, EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.transactionId sameElements tx.id) shouldBe true
    }
  }

  property("Wallet should scan persistent with recipient from wallet") {
    forAll(simpleBoxTransactionGen) { txIn =>
      val toWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Long)] =
        txIn.to.map(p => (ss.publicImage, p._2 + 1))
      val tx = txIn.copy(to = toWithMyPubkey)

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), fs.publicImage, EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.transactionId sameElements tx.id) shouldBe true
    }
  }

}
