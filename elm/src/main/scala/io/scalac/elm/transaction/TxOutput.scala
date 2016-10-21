package io.scalac.elm.transaction

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

case class TxOutput(id: Array[Byte], value: Long, proposition: PublicKey25519Proposition) extends Box[PublicKey25519Proposition] {
  override def bytes: Array[Byte] = ???
}
