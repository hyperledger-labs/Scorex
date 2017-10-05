package hybrid.transaction

import examples.commons.SimpleBoxTransaction
import hybrid.HybridGenerators
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.transaction.state.PrivateKey25519Companion


class TransactionSuite extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {

  property("SimpleBoxTransaction preservers inputs ids") {
    forAll(noncedBoxWithKeyListGen, noncedBoxWithKeyListGen) { case (boxesWithKeysIn, boxesWithKeysOut) =>

      val inputs = boxesWithKeysIn.map { case (box, k) => k -> box.nonce }.toIndexedSeq

      boxesWithKeysIn.foreach { case (box, k) =>
        assert(PrivateKey25519Companion.owns(k, box))
      }

      val boxIds = boxesWithKeysIn.map(_._1.id).map(ByteArrayWrapper.apply)

      val to = boxesWithKeysOut.map { case (box, _) =>
        box.proposition -> box.value
      }.toIndexedSeq

      val tx: SimpleBoxTransaction = SimpleBoxTransaction(inputs, to, 0, 0)

      val outKeys = boxesWithKeysOut.map(_._2).map(_.publicKeyBytes).map(ByteArrayWrapper.apply)

      tx.newBoxes.foreach { newBox =>
        assert(outKeys.contains(ByteArrayWrapper(newBox.proposition.pubKeyBytes)))
      }

      tx.boxIdsToOpen.map(ByteArrayWrapper.apply).forall { bid =>
        boxIds.contains(bid)
      } shouldBe true
    }
  }

}
