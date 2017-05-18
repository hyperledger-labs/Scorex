package examples.spv

import com.google.common.primitives.{Bytes, Shorts}
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.Try

case class KMZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length == k, "Wrong suffix length")
    //TODO should be true?
    //    prefixProofs.foreach(p => require(p.length == m, s"${p.length} == $m"))

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
    val prefixHeaders: Map[String, Header] = obj.prefixProofs.flatten.map(h => h.encodedId -> h).toMap
    val prefixHeadersBytes: Array[Byte] = prefixHeaders.flatMap { h =>
      val bytes = h._2.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    }.toArray
    val prefixIdsBytes: Array[Byte] = scorex.core.utils.concatBytes(obj.prefixProofs.map { chain =>
      Shorts.toByteArray(chain.length.toShort) ++ chain.flatMap(_.id)
    })

    Bytes.concat(Array(obj.m.toByte, obj.k.toByte),
      Shorts.toByteArray(obj.suffix.head.bytes.length.toShort),
      obj.suffix.head.bytes,
      suffixTailBytes,
      Shorts.toByteArray(prefixHeaders.size.toShort),
      prefixHeadersBytes,
      Shorts.toByteArray(obj.prefixProofs.length.toShort),
      prefixIdsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[KMZProof] = Try {

    val m = bytes.head
    val k = bytes(1)
    val headSuffixLength = Shorts.fromByteArray(bytes.slice(2, 4))
    val headSuffix = HeaderSerializer.parseBytes(bytes.slice(4, 4 + headSuffixLength)).get

    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == k) (index, acc.reverse)
      else {
        val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
        val interlinks = Algos.constructInterlinkVector(acc.head)
        parseSuffixes(index + 2 + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }
    var (index, suffix) = parseSuffixes(4 + headSuffixLength, Seq(headSuffix))

    val prefixHeadersLength: Int = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val prefixHeaders: Map[String, Header] = ((0 until prefixHeadersLength) map { i: Int =>
      val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
      val header = HeaderSerializer.parseBytes(bytes.slice(index + 2, index + 2 + l)).get
      index = index + 2 + l
      header.encodedId -> header
    }).toMap


    val prefixProofsLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
    index = index + 2
    val prefixIds: Seq[Seq[String]] = (0 until prefixProofsLength) map { _ =>
      val l = Shorts.fromByteArray(bytes.slice(index, index + 2))
      index = index + 2
      (1 to l).map { i =>
        val b = bytes.slice(index, index + 32)
        index = index + 32
        Base58.encode(b)
      }
    }

    val prefixProofs: Seq[Seq[Header]] = prefixIds.map(_.map(id => prefixHeaders(id)))
    KMZProof(m, k, prefixProofs, suffix)
  }
}