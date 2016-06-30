package scorex.block

trait ConsensusData {
  import ConsensusData.BlockId

  val BlockIdLength: Int

  val parentId: BlockId
}

object ConsensusData {
  type BlockId = Array[Byte]
}
