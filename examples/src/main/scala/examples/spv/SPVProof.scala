package examples.spv

import scala.util.Try

case class SPVProof(m: Int, k: Int, difficulty: BigInt, interchain: Seq[Header], suffix: Seq[Header]) extends
  Comparable[SPVProof]
  with Ordered[SPVProof] {

  lazy val validate: Try[Unit] = Try {
    require(suffix.length == k, s"${suffix.length} == $k")
    require(interchain.length >= m, s"${interchain.length} >= $m")
    interchain.foreach(b => require(b.realDifficulty >= difficulty, s"$b: ${b.realDifficulty} >= $difficulty"))
  }

  override def compare(that: SPVProof): Int = {
    if (that.validate.isFailure) {
      //TODO what is both are isFailure?
      1
    } else if (this.validate.isFailure) {
      -1
    } else {
      val ourIndex = this.suffix.reverse.indexWhere(h => that.suffix.exists(_.id sameElements h.id))
      if(ourIndex >= 0 ) {
        //there is common block in suffix
        val theirIndex = that.suffix.reverse.indexWhere(h => this.suffix.exists(_.id sameElements h.id))
        ourIndex - theirIndex
      } else {
        //no common block in suffix
        ???
      }
    }
  }
}

