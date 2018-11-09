package hybrid.history

import examples.commons._
import examples.hybrid.blocks._
import hybrid.HybridGenerators
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, Outcome, fixture}
import scorex.core.utils.ScorexEncoding
import scorex.testkit.utils.FileUtils
import scorex.util.ModifierId

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
        .map(b => (ByteArrayWrapper(b.id), ByteArrayWrapper(PublicKey25519NoncedBoxSerializer.toBytes(b)))).toList
      blocksStorage.update(idToBAW(tx.id), boxIdsToRemove, boxesToAdd)
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

        blocksStorage.rollback(idToBAW(head.id))
        checkTx(head)
      }
    }
  }

  property("writeBlock() test") { blocksStorage =>
    def writeBlock(b: HybridBlock): Unit = {
      b match {
        case block: PowBlock =>
          blocksStorage.update(
            idToBAW(b.id),
            Seq(),
            Seq(idToBAW(b.id) -> ByteArrayWrapper(PowBlock.ModifierTypeId +: PowBlockSerializer.toBytes(block))))

        case block: PosBlock =>
          PosBlock.ModifierTypeId -> PowBlockSerializer
          blocksStorage.update(
            idToBAW(b.id),
            Seq(),
            Seq(idToBAW(b.id) -> ByteArrayWrapper(PosBlock.ModifierTypeId +: PosBlockSerializer.toBytes(block))))
      }
    }

    var ids: Seq[ModifierId] = Seq()

    forAll(powBlockGen) { block =>
      ids = block.id +: ids
      writeBlock(block)
      blocksStorage.get(idToBAW(block.id)).isDefined shouldBe true
    }
    ids.foreach { id =>
      blocksStorage.get(idToBAW(id)) match {
        case None =>
          throw new Error(s"Id ${encoder.encodeId(id)} not found")
        case Some(_) => ()
      }
    }
  }
}
