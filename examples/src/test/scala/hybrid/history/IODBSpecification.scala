package hybrid.history

import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import hybrid.HybridGenerators
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, Outcome, fixture}
import scorex.core.ModifierId
import scorex.core.utils.ScorexEncoding
import scorex.testkit.utils.FileUtils

@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
class IODBSpecification extends fixture.PropSpec
  with GeneratorDrivenPropertyChecks
  with Matchers
  with HybridGenerators
  with FileUtils
  with ScorexEncoding {

  type FixtureParam = LSMStore

  override def withFixture(test: OneArgTest): Outcome = {
    val fixture = new LSMStore(createTempDir)
    try test(fixture)
    finally {}
  }


  property("Rollback should not touch keys before") { blocksStorage =>
    def writeTx(tx: SimpleBoxTransaction): Unit = {
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

  property("writeBlock() test") { blocksStorage =>
    def writeBlock(b: HybridBlock): Unit = {
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
          throw new Error(s"Id ${encoder.encode(id)} not found")
        case Some(_) => ()
      }
    }
  }
}
