package scorex.api.http

import io.circe.Json
import scorex.block.{Block, ConsensusData}
import scorex.consensus.BlockChain
import scorex.transaction.box.Proposition

trait CommonApiFunctions {
  import ApiError._

  protected[api] def withBlock[P <: Proposition, CData <: ConsensusData, B <: Block[P, CData, _]](history: BlockChain[P, CData, B], encodedId: String)
                                                      (action: B => Json): Json =
    history.blockById(encodedId) match {
      case Some(block) => action(block)
      case None => blockNotExists
    }
}