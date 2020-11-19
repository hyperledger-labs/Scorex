package hybrid.transaction

import examples.commons.SimpleBoxTransaction
import hybrid.HybridGenerators
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.core.transaction.state.PrivateKey25519Companion


class TransactionSuite extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with HybridGenerators {

  property("SimpleBoxTransaction preservers inputs ids") {
    forAll(noncedBoxWithKeyListGen, noncedBoxWithKeyListGen) { case (boxesWithKeysIn, boxesWithKeysOut) =>

      val inputs = boxesWithKeysIn.map { case (box, k) => k -> box.nonce }.toIndexedSeq

      boxesWithKeysIn.foreach { case (box, k) =>
        PrivateKey25519Companion.owns(k, box) shouldBe true
      }

      val boxIds = boxesWithKeysIn.map(_._1.id).map(ByteArrayWrapper.apply)

      val to = boxesWithKeysOut.map { case (box, _) =>
        box.proposition -> box.value
      }.toIndexedSeq

      val tx: SimpleBoxTransaction = SimpleBoxTransaction(inputs, to, 0, 0)

      val outKeys = boxesWithKeysOut.map(_._2).map(_.publicKeyBytes).map(ByteArrayWrapper.apply)

      tx.newBoxes.foreach { newBox =>
        outKeys.contains(ByteArrayWrapper(newBox.proposition.pubKeyBytes)) shouldBe true
      }

      tx.boxIdsToOpen.map(ByteArrayWrapper.apply).forall { bid =>
        boxIds.contains(bid)
      } shouldBe true
    }
  }

}
