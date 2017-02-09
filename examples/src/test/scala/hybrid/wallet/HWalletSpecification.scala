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
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.Random

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

  property("Wallet should generate same keys") {
    val keys = HWallet.readOrGenerate(settings, "genesis", "e", 10).publicKeys
    keys.map(_.toString).mkString(",") shouldBe "3g21Bv22ES7suDuq3cVGCQqhCCqhtePE5KNz1aCUnQmbtauPw9,4SexWNVTphERBpx2QfWKP2Fz1QYoee91g8rzMBAJExUF5fDuGt,3SHtEdr7tvjA8o27PTAkcBxEjWMbjSQAYH5knBGYLAC7AKJCfg,4BA9MhA62fm5AjbVMvvRrvomfsrMYyijYALP45Eyooo2JvKVb9,3y11aj63n4uvWa3dXh2aQ84YJYQvEFoB1YEVhn2aSXDhVRfVrE,3uQJ736znugcTe76cfyQPcwE1LvA3Ka2Ec2F8FbMCz8xz5NnzF,3pxZkRDUarzPHwaRDbCinhKUk4vEsRZBUFVyBRWNMsHp6v6Naa,4SwRrrULrttmFncRZP8YFMn27oZV8vTFffwt92uV6foiYCcZUb,3zgP2RAUMdBdDraE6E4VokvPC8DtSEACx9c4Dtd6WNVUp5gasU,3aXBqeGdvo6VuWiTyzpuf2skoVZqZ1ousrxxUBApbrzHuNPtxP"
  }

  property("Wallet should add boxes where he is recipient") {
    forAll(simpleBoxTransactionGen, noncedBoxGen) { (txIn, box) =>
      val toWithMyPubkey: IndexedSeq[(PublicKey25519Proposition, Long)] =
        txIn.to.map(p => (ss.publicImage, p._2 + 1))
      val tx = txIn.copy(to = toWithMyPubkey)

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, EmptySignature)
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

      val pb = PosBlock(EmptyBytes, System.currentTimeMillis(), Seq(tx), box, EmptySignature)
      val boxes = w.scanPersistent(pb).boxes()
      boxes.exists(b => b.box.id sameElements boxToRemove.id) shouldBe false
    }
  }


}
