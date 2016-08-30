package scorex.core.api.http

import io.circe.Json
import scorex.core.block.Block
import scorex.core.consensus.BlockChain
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

trait CommonApiFunctions {

  import ApiError._

  protected[api] def withBlock[P <: Proposition, TX <: Transaction[P, TX]]
  (history: BlockChain[P, TX], encodedId: String)
  (action: Block[P, TX] => Json): Json =
    history.blockById(encodedId) match {
      case Some(block) => action(block)
      case None => blockNotExists
    }
}