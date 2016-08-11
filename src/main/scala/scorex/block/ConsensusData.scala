package scorex.block

import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, JsonSerializable}

/**
  * ConsensusData is about data to be stored into a block header, and be used in order to verify
  * correctness of a block generation and also history consistence.
  */
trait ConsensusData extends BytesSerializable with JsonSerializable {

  val version: Byte = 0: Byte
  val BlockIdLength: Int

  //TODO is it ok to have it here?
  def score(): BigInt

  /**
   * A block have some reference to transactional data to fix it
   */
  val transactionalId: ConsensusData.BlockId

  /**
    * A block always refers to some previous block, so parent ID is to be stored into a block
    */
  val parentId: ConsensusData.BlockId

  /**
    * A block always have some id that identifies it
    */
  val id: ConsensusData.BlockId

  lazy val encodedId: String = Base58.encode(id)
}

object ConsensusData {
  type BlockId = Array[Byte]
}