package scorex.block

import scorex.serialization.{BytesSerializable, JsonSerializable}

/**
  * ConsensusData is about data to be stored into a block header, and be used in order to verify
  * correctness of a block generation and also history consistence.
  */
trait ConsensusData extends BytesSerializable with JsonSerializable {
  /
  val BlockIdLength: Int

  /**
    * A block always refers to some previous block, so parent ID is to be stored into a block
    */
  val parentId: ConsensusData.BlockId
}

object ConsensusData {
  type BlockId = Array[Byte]
}