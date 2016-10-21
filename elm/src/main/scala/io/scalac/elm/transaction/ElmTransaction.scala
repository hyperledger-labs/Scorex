package io.scalac.elm.transaction

import play.api.libs.json.JsObject
import scorex.core.NodeViewModifierCompanion
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.box.{Box, BoxUnlocker}

import scala.util.Try

object ElmTransaction extends NodeViewModifierCompanion[ElmTransaction] {
  val TransactionLength = ???

  override def bytes(m: ElmTransaction): Array[Byte] = ???

  override def parse(bytes: Array[Byte]): Try[ElmTransaction] = ???
}


case class ElmTransaction(inputs: Seq[TxInput], outputs: Seq[TxOutput], fee: Long, timestamp: Long, signature: Array[Byte])
  extends Transaction[PublicKey25519Proposition] {

  override type M = ElmTransaction

  override val messageToSign = ElmTransaction.bytes(this)

  override def companion: NodeViewModifierCompanion[ElmTransaction] = ElmTransaction
  override def json: JsObject = ???
}
