package io.scalac.elm.transaction

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.scalac.elm.serialization.ByteSerialization
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

case class TxOutput(value: Long, proposition: PublicKey25519Proposition)
  extends Box[PublicKey25519Proposition] with ByteSerialization[TxOutput] {

  val id = UUID.randomUUID().toString.getBytes(StandardCharsets.US_ASCII)

  def bytes: Array[Byte] = bytes(this)
}
