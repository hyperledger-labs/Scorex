package examples.spv

import com.google.common.primitives.{Bytes, Shorts}
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

case class KMZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length >= k, "Wrong suffix length")
    prefixProofs.foreach(p => require(p.length == m, s"${p.length} == $m"))

    suffix.foldRight(Array[Byte]()) { (a, b) =>
      if (b.nonEmpty) require(b sameElements a.id)
      a.parentId
    }
  }
}

object KMZProofSerializer extends Serializer[KMZProof] {


  override def toBytes(obj: KMZProof): Array[Byte] = {
    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixTailBytes = scorex.core.utils.concatBytes(obj.suffix.tail.map { h =>
      HeaderSerializer.bytesWithoutInterlinks(h)
    })
    val prefixHeaders: Map[String, Header] = obj.prefixProofs.flatten.map(h => h.encodedId -> h).toMap
    val prefixHeadersBytes: Array[Byte] = prefixHeaders.flatMap { h =>
      val bytes = h._2.bytes
      Bytes.concat(Shorts.toByteArray(bytes.length.toShort), bytes)
    }.toArray
    val prefixIdsBytes: Array[Byte] = scorex.core.utils.concatBytes(obj.prefixProofs.map { chain =>
      Shorts.toByteArray(chain.length.toShort) ++ chain.flatMap(_.id)
    })

    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixHead = obj.suffix.head
    Bytes.concat(Array(obj.m.toByte, obj.k.toByte),
      Shorts.toByteArray(suffixHead.bytes.length.toShort),
      suffixHead.bytes,
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
    val l = HeaderSerializer.BytesWithoutInterlinksLength

    @tailrec
    def parseSuffixes(index: Int, acc: Seq[Header]): (Int, Seq[Header]) = {
      if (acc.length == k) (index, acc.reverse)
      else {
        val headerWithoutInterlinks = HeaderSerializer.parseBytes(bytes.slice(index, index + l)).get
        // TODO: fixme, What should we do if `acc` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val interlinks = SpvAlgos.constructInterlinkVector(acc.head)
        parseSuffixes(index + l, headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
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