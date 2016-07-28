package scorex.block

import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.Transaction

trait TransactionalData[TX <: Transaction[_, TX]] extends BytesSerializable with JsonSerializable {
  val mbTransactions: Option[Traversable[TX]]

  lazy val headerOnly = mbTransactions.isDefined
}

