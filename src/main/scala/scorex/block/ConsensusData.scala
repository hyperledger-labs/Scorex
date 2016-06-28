package scorex.block

import shapeless.HList

trait ConsensusData {

  import ConsensusData.BlockId

  val BlockIdLength: Int

  type ConsensusFields <: HList

  val consensusFields: ConsensusFields

  val parentId: BlockId
}

object ConsensusData {
  type BlockId = Array[Byte]
}
