package examples.spv

import com.google.common.primitives.{Bytes, Shorts}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer

import scala.annotation.tailrec
import scala.util.Try

case class KLS16Proof(m: Int,
                      k: Int,
                      i: Int,
                      innerchain: Seq[Header],
                      suffix: Seq[Header]) extends Comparable[KLS16Proof] with Ordered[KLS16Proof] {

  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, s"${suffix.length} == $k")

    suffix.foldRight[Option[ModifierId]](None) { (a, b) =>
      b foreach (id => require(id == a.id))
      Some(a.parentId)
    }

    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val firstSuffix = suffix.head
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val lastInnerchain = innerchain.last
    require(firstSuffix.interlinks(i) == lastInnerchain.id)


    val difficulty: BigInt = Constants.InitialDifficulty * Math.pow(2, i).toInt
    require(innerchain.length >= m, s"${innerchain.length} >= $m")
    innerchain.foreach(b => require(b.realDifficulty >= difficulty, s"$b: ${b.realDifficulty} >= $difficulty"))

    innerchain.foldRight[Option[ModifierId]](None) { (a, b) =>
      b foreach (id => require(id == a.id))
      //last element may not contain a.interlinks(i)
      Try(a.interlinks(i)).toOption
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
      val ourIndex = this.suffix.reverse.indexWhere(h => that.suffix.exists(_.id == h.id))
      if (ourIndex >= 0) {
        //there is common block in suffix
        val theirIndex = that.suffix.reverse.indexWhere(h => this.suffix.exists(_.id == h.id))
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
    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val interchainBytes = scorex.core.utils.concatBytes(obj.innerchain.map { h =>
      val bytes = h.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })

    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixHead = obj.suffix.head
    Bytes.concat(Array(obj.m.toByte, obj.k.toByte, obj.i.toByte),
      Shorts.toByteArray(suffixHead.bytes.length.toShort),
      suffixHead.bytes,
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
    @tailrec
    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == k) (index, acc.reverse)
      else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        //TODO: fixme, What should happen if `acc` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val interlinks = SpvAlgos.constructInterlinkVector(acc.head)
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

