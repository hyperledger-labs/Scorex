package hybrid.history

import java.io.File

import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import hybrid.HybridGenerators
import io.iohk.iodb.Store._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.NodeViewModifier._
import scorex.crypto.encode.Base58

import scala.util.Random

class IODBSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators {


  val iFile = new File(s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}")
  iFile.mkdirs()
  val blocksStorage = new LSMStore(iFile)

  property("Rollback should not touch keys before") {
    def writeTx(tx: SimpleBoxTransaction) = {
      //      val boxIdsToRemove: Iterable[ByteArrayWrapper] = tx.boxIdsToOpen.map(id => ByteArrayWrapper(id))
      val boxIdsToRemove: Iterable[ByteArrayWrapper] = Seq()
      val boxesToAdd: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = tx.newBoxes
        .map(b => (ByteArrayWrapper(b.id), ByteArrayWrapper(b.bytes))).toList
      blocksStorage.update(ByteArrayWrapper(tx.id), boxIdsToRemove, boxesToAdd)
    }

    def checkTx(tx: SimpleBoxTransaction): Unit = {
      tx.newBoxes.foreach(b => require(blocksStorage.get(ByteArrayWrapper(b.id)).isDefined))
    }

    forAll(Gen.nonEmptyListOf(simpleBoxTransactionGen)) { txs =>
      whenever(txs.length >= 2) {
        val head = txs.head
        writeTx(head)
        checkTx(head)
        txs.tail.foreach(tx => writeTx(tx))
        txs.foreach(tx => checkTx(tx))

        blocksStorage.rollback(ByteArrayWrapper(head.id))
        checkTx(head)


      }
    }
  }

  property("writeBlock() test") {
    def writeBlock(b: HybridBlock) = {
      val typeByte = b match {
        case _: PowBlock =>
          PowBlock.ModifierTypeId
        case _: PosBlock =>
          PosBlock.ModifierTypeId
      }

      blocksStorage.update(
        ByteArrayWrapper(b.id),
        Seq(),
        Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
    }

    var ids: Seq[ModifierId] = Seq()

    forAll(powBlockGen) { block =>
      ids = block.id +: ids
      writeBlock(block)
      blocksStorage.get(ByteArrayWrapper(block.id)).isDefined shouldBe true
    }
    ids.foreach { id =>
      blocksStorage.get(ByteArrayWrapper(id)) match {
        case None =>
          throw new Error(s"Id ${Base58.encode(id)} not found")
        case Some(v) =>
      }
    }
  }

}
