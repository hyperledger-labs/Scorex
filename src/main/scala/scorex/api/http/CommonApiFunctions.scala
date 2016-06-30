package scorex.api.http

import io.circe.Json
import scorex.block.{Block, ConsensusData, TransactionalData}
import scorex.consensus.BlockChain
import scorex.transaction.Transaction
import scorex.transaction.box.proposition.Proposition

trait CommonApiFunctions {

  import ApiError._

  protected[api] def withBlock[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  (history: BlockChain[P, TX, TData, CData], encodedId: String)
  (action: Block[P, CData, TData] => Json): Json =
    history.blockById(encodedId) match {
      case Some(block) => action(block)
      case None => blockNotExists
    }
}
