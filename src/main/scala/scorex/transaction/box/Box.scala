package scorex.transaction.box

import scorex.serialization.BytesSerializable
import scorex.transaction.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait Box[P <: Proposition] extends BytesSerializable {
  val proposition: P

  val value: Long

  val id: Array[Byte]
}

