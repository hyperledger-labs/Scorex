package hybrid.history

import java.io.File

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import hybrid.HybridGenerators
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
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

  property("writeBlock() test") {
    val iFile = new File(s"/tmp/scorex/scorextest-${Random.nextInt(10000000)}")
    iFile.mkdirs()
    val blocksStorage = new LSMStore(iFile)
    def writeBlock(b: HybridPersistentNodeViewModifier) = {
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
