package scorex.block

import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.Transaction

/**
  * Part of block which contains transaction as well as some metadata
  * about them (e.g. Merkle tree root value)
  * If this is a part of blockheader, not a full block, mbTransactions are about None
  * and thus headerOnly is true
  *
  * @tparam TX - base type of transactions to be packed into a block
  */
trait TransactionalData[TX <: Transaction[_, TX]] extends BytesSerializable with JsonSerializable {
  val mbTransactions: Option[Traversable[TX]]
  lazy val headerOnly = mbTransactions.isDefined
}

