package scorex.block

import io.circe.Json
import scorex.serialization.{BytesSerializable, JsonSerializable}

trait ConsensusData extends BytesSerializable with JsonSerializable {
  import ConsensusData.BlockId

  val BlockIdLength: Int

  val parentId: BlockId

  override val bytes: Array[Byte] = ???

  override val json: Json = ???
}

object ConsensusData {
  type BlockId = Array[Byte]
}
