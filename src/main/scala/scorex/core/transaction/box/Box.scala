package scorex.core.transaction.box

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait Box[P <: Proposition] extends BytesSerializable {
  val proposition: P

  val value: Long

  val id: Array[Byte]
}

