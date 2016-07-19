package scorex.block

import scorex.serialization.{BytesSerializable, JsonSerializable}

trait ConsensusData extends BytesSerializable with JsonSerializable {

  import ConsensusData.BlockId

  val BlockIdLength: Int

  val parentId: BlockId

}

object ConsensusData {
  type BlockId = Array[Byte]
}
