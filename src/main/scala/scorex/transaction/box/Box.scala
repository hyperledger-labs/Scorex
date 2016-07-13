package scorex.transaction.box

import scorex.serialization.BytesSerializable
import scorex.transaction.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait Box[L <: Proposition] extends BytesSerializable {
  val lock: L

  val id: Array[Byte]

  val value: Long
}

