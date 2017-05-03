package examples.spv

import com.google.common.primitives.{Bytes, Shorts}
import scorex.core.serialization.Serializer

import scala.util.Try

case class KLS16Proof(m: Int,
                      k: Int,
                      i: Int,
                      innerchain: Seq[Header],
                      suffix: Seq[Header]) extends Comparable[KLS16Proof] with Ordered[KLS16Proof] {

  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, s"${suffix.length} == $k")

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }

    require(suffix.head.interlinks(i) sameElements innerchain.last.id)

    val difficulty: BigInt = Constants.InitialDifficulty * Math.pow(2, i).toInt
    require(innerchain.length >= m, s"${innerchain.length} >= $m")
    innerchain.foreach(b => require(b.realDifficulty >= difficulty, s"$b: ${b.realDifficulty} >= $difficulty"))

    innerchain.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) {
        require(b sameElements a.id)
      }
      //last element may not contain a.interlinks(i)
      Try(a.interlinks(i)).getOrElse(Array.fill(32)(0.toByte))
    }

    //TODO check that genesis links are correct
  }

  override def compare(that: KLS16Proof): Int = {
    if (that.valid.isFailure) {
      //TODO what is both are isFailure?
      1
    } else if (this.valid.isFailure) {
      -1
    } else {
      val ourIndex = this.suffix.reverse.indexWhere(h => that.suffix.exists(_.id sameElements h.id))
      if (ourIndex >= 0) {
        //there is common block in suffix
        val theirIndex = that.suffix.reverse.indexWhere(h => this.suffix.exists(_.id sameElements h.id))
        ourIndex - theirIndex
      } else {
        //no common block in suffix
        val b = ??? //most recent common block

        ???
      }
    }
  }

}

object KLS16ProofSerializer extends Serializer[KLS16Proof] {
  override def toBytes(obj: KLS16Proof): Array[Byte] = {
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val interchainBytes = scorex.core.utils.concatBytes(obj.innerchain.map { h =>
      val bytes = h.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    Bytes.concat(Array(obj.m.toByte, obj.k.toByte, obj.i.toByte),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(obj.innerchain.length.toShort),
      interchainBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[KLS16Proof] = Try {
    val m = bytes.head
    val k = bytes(1)
    val i = bytes(2)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(3, 5))
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(5, 5 + headSuffixLength)).get
    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == k) (index, acc.reverse)
      else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = Algos.constructInterlinkVector(acc.head)
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }

    }
    var (index, suffix) = parseSuffixes(5 + headSuffixLength, Seq(headSuffix))
    val interchainLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val interchain = (0 until interchainLength) map { _ =>
      val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
      val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
      index = index + 2 + l
      header
    }
    KLS16Proof(m, k, i, interchain, suffix)
  }
}

