package scorex.core.api.http

import io.circe.Json
import scorex.core.block.Block
import scorex.core.consensus.BlockChain
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

trait CommonApiFunctions {

  import ApiError._

  protected[api] def withBlock[P <: Proposition, TX <: Transaction[P], B <: Block[P, TX], BT <: BlockChain[P, TX, B, BT]]
  (history: BlockChain[P, TX, B, BT], encodedId: String)
  (action: B => Json): Json =
    history.blockById(encodedId) match {
      case Some(block) => action(block)
      case None => blockNotExists
    }
}