package examples.spv

import scorex.core._
import scorex.util.serialization.{Reader, Writer}
import scorex.core.serialization.ScorexSerializer
import scorex.core.utils.ScorexEncoding

import scala.annotation.tailrec
import scala.util.Try
import scorex.util.Extensions._

case class KMZProof(m: Int, k: Int, prefixProofs: Seq[Seq[Header]], suffix: Seq[Header]) {
  lazy val valid: Try[Unit] = Try {
    require(suffix.length >= k, "Wrong suffix length")
    prefixProofs.foreach(p => require(p.length == m, s"${p.length} == $m"))

    suffix.foldRight[Option[ModifierId]](None) { (a, b) =>
      b foreach (id => require(id == a.id))
      Some(a.parentId)
    }
  }
}

object KMZProofSerializer extends ScorexSerializer[KMZProof] with ScorexEncoding {

  override def serialize(obj: KMZProof, w: Writer): Unit = {

    w.put(obj.m.toByte)
    w.put(obj.k.toByte)

    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixHead = obj.suffix.head
    HeaderSerializer.serialize(suffixHead, w)

    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixTail = obj.suffix.tail
    suffixTail.map { h =>
      HeaderSerializer.serializeWithoutIterlinks(h, w)
    }

    val prefixHeaders: Map[String, Header] = obj.prefixProofs.flatten.map(h => h.encodedId -> h).toMap
    w.putShort(prefixHeaders.size.toShortExact)
    prefixHeaders.foreach { h =>
      HeaderSerializer.serialize(h._2, w)
    }

    w.putShort(obj.prefixProofs.length.toShortExact)
    obj.prefixProofs.foreach { chain =>
      w.putShort(chain.length.toShortExact)
      chain.foreach { c =>
        w.putBytes(idToBytes(c.id))
      }
    }
  }

  override def parse(r: Reader): KMZProof = {
    val m = r.getByte()
    val k = r.getByte()
    val headSuffix = HeaderSerializer.parse(r)

    @tailrec
    def parseSuffixes(acc: Seq[Header]): Seq[Header] = {
      if (acc.length == k) {
        acc.reverse
      } else {
        val headerWithoutInterlinks = HeaderSerializer.parseWithoutInterlinks(r)
        // TODO: fixme, What should we do if `acc` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val interlinks = SpvAlgos.constructInterlinkVector(acc.head)
        parseSuffixes(headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    val suffix = parseSuffixes(Seq(headSuffix))

    val prefixHeadersLength: Int = r.getShort()
    val prefixHeaders: Map[String, Header] = ((0 until prefixHeadersLength) map { i: Int =>
      val header = HeaderSerializer.parse(r)
      header.encodedId -> header
    }).toMap


    val prefixProofsLength = r.getShort()
    val prefixIds: Seq[Seq[String]] = (0 until prefixProofsLength) map { _ =>
      val l = r.getShort()
      (1 to l).map { i =>
        val b = r.getBytes(32)
        encoder.encode(b)
      }
    }

    val prefixProofs: Seq[Seq[Header]] = prefixIds.map(_.map(id => prefixHeaders(id)))
    KMZProof(m, k, prefixProofs, suffix)
  }
}