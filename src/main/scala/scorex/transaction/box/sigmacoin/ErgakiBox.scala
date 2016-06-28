package scorex.transaction.box.sigmacoin

import scorex.transaction.box.Box
import scorex.transaction.box.proposition.HeightOpenProposition

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
