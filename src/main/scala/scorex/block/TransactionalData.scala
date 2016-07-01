package scorex.block

import io.circe.Json
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.Transaction

trait TransactionalData[TX <: Transaction[_, TX]] extends BytesSerializable with JsonSerializable {
  val mbTransactions: Option[Traversable[TX]]

  val headerOnly = mbTransactions.isDefined

  override val bytes: Array[Byte] = ???

  override val json: Json = ???
}

