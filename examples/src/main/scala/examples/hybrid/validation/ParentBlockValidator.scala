package examples.hybrid.validation

import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HistoryStorage
import scorex.core.ModifierId
import scorex.core.block.BlockValidator
import scorex.crypto.encode.Base58

import scala.util.Try

class ParentBlockValidator(storage: HistoryStorage)
  extends BlockValidator[HybridBlock] {

  def validate(block: HybridBlock): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock => if (!storage.isGenesis(powBlock)) {
        //check PoW parent id ???
        require(storage.modifierById(ModifierId !@@ powBlock.parentId.arr).isDefined, s"Parent ${Base58.encode(powBlock.parentId.arr)} missed")
        //check referenced PoS block exists as well
        val posBlock = storage.modifierById(powBlock.prevPosId).get

        //check referenced PoS block points to parent PoW block
        require(posBlock.parentId.arr sameElements posBlock.parentId.arr, "ref rule broken")
      }
      case posBlock: PosBlock =>
        //check PoW block exists
        require(storage.modifierById(ModifierId !@@ posBlock.parentId.arr).isDefined)
    }
  }

}
