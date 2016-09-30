package scorex.core.transaction.box

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition

/**
  * Box is a state element locked by some proposition.
  */
trait Box[P <: Proposition] extends BytesSerializable {
  type Amount = Long

  val value: Amount
  val proposition: P

  val id: Array[Byte]
}

