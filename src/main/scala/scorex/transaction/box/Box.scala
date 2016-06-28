package scorex.transaction.box

import scorex.serialization.BytesSerializable

/**
  * Box is a state element locked by some proposition.
  */
//todo: Box is a Functor?
trait Box[L <: Proposition] extends BytesSerializable {
  val lock: L

  val id: Array[Byte]

  val value: Long
}

//todo: move SigmaBox/ErgakiBox to the SigmaCoin module
trait SigmaBox[SL <: SigmaProposition] extends Box[SL]

trait ErgakiBox[SL <: SigmaProposition] extends SigmaBox[SL] {

  require(value > 0)
  //todo: ?

  //garbage-collecting lock
  val gcLock: HeightOpenProposition

  //min box size to have zero-fee
  val SMin = 0

  def minFee(currentHeight: Int): Int =
    (bytes.length - SMin + 1) * (gcLock.height - currentHeight)

  val memo: Array[Byte]
}