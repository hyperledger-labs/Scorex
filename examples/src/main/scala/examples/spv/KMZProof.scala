package examples.spv

import com.google.common.primitives.{Bytes, Shorts}
import scorex.core.serialization.Serializer

import scala.util.Try

case class KMZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, "Wrong suffix length")

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }
  }
}

object KMZProofSerializer extends Serializer[KMZProof] {
  override def toBytes(obj: KMZProof): Array[Byte] = {
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      val bytes = HeaderSerializer.bytesWithoutInterlinks(h)
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    })
    val prefixProofsBytes: Array[Byte] = scorex.core.utils.concatBytes(obj.prefixProofs.map { chain =>
      val a: Array[Byte] = Shorts.toByteArray(chain.length.toShort) ++ chain.flatMap { h =>
        val bytes = h.bytes
        Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
      }
      a
    })
    Bytes.concat(Array(obj.m.toByte, obj.k.toByte),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(obj.prefixProofs.length.toShort),
      prefixProofsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[KMZProof] = Try {
    val m = bytes.head
    val k = bytes(1)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(2, 4))
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(4, 4 + headSuffixLength)).get
    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == m) (index, acc.reverse)
      else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = Algos.constructInterlinkVector(acc.head)
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }

    }
    var (index, suffix) = parseSuffixes(4 + headSuffixLength, Seq(headSuffix))

    val prefixProofsLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val prefixProofs = (0 until prefixProofsLength) map { _ =>
      val headersN = Shorts.fromByteArray(bytes.slice(index, index + 2))
      index = index + 2
      (0 until headersN) map { _ =>
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        index = index + 2 + l
        header
      }
    }
    KMZProof(m, k, prefixProofs, suffix)
  }
}