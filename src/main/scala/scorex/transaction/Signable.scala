package scorex.transaction

import scorex.serialization.BytesSerializable
import scorex.transaction.proof.Proof

trait Signable extends BytesSerializable {
  val proof: Proof[_]

  override lazy val bytes: Array[Byte] = messageToSign ++ proof.bytes

  def messageToSign: Array[Byte]
}