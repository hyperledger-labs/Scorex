package io.scalac.elm.transaction


import io.circe._
import io.circe.generic.auto._
import io.scalac.elm.serialization.Serialization
import scorex.core.NodeViewModifierCompanion
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition


object ElmTransaction extends Serialization[ElmTransaction] {
  val codec = getCodec
}

case class ElmTransaction(inputs: Seq[TxInput], outputs: Seq[TxOutput], fee: Long, timestamp: Long, signature: Array[Byte])
  extends Transaction[PublicKey25519Proposition] {

  override type M = ElmTransaction

  override lazy val messageToSign = ElmTransaction.bytes(this)

  override def companion: NodeViewModifierCompanion[ElmTransaction] = ElmTransaction
  override def json: Json = ElmTransaction.toJson(this)
}

