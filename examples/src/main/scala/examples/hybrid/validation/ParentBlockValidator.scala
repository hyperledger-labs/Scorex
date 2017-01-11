package examples.hybrid.validation

import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import examples.hybrid.history.HistoryStorage

import scala.util.Try

class ParentBlockValidator(storage: HistoryStorage)
  extends BlockValidator {

  def validate(block: HybridPersistentNodeViewModifier): Try[Unit] = Try {
    block match {
      case powBlock: PowBlock => if (!storage.isGenesis(powBlock)) {
        //check PoW parent id ???
        storage.modifierById(powBlock.parentId).get
        //check referenced PoS block exists as well
        val posBlock = storage.modifierById(powBlock.prevPosId).get

        //check referenced PoS block points to parent PoW block
        require(posBlock.parentId sameElements posBlock.parentId, "ref rule broken")
      }
      case posBlock: PosBlock =>
        //check PoW block exists
        require(storage.modifierById(posBlock.parentId).isDefined)
    }
  }

}
