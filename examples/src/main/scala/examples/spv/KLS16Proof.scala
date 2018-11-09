package examples.spv

import scorex.core.newserialization.{ScorexReader, ScorexSerializer, ScorexWriter}
import scorex.util.ModifierId

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

object KLS16ProofSerializer extends ScorexSerializer[KLS16Proof] {

  override def serialize(obj: KLS16Proof, w: ScorexWriter): Unit = {
    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixHead = obj.suffix.head
    val suffixHeadWriter = w.newWriter()
    HeaderSerializer.serialize(suffixHead, suffixHeadWriter)

    w.put(obj.m.toByte)
    w.put(obj.k.toByte)
    w.put(obj.i.toByte)
    w.putShort(suffixHeadWriter.length().toShort)
    w.append(suffixHeadWriter)

    // TODO: fixme, What should we do if `obj.suffix` is empty?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val suffixTail = obj.suffix.tail
    suffixTail.foreach { h =>
      val hw = w.newWriter()
      HeaderSerializer.serializeWithoutItrelinks(h, hw)
      w.putShort(hw.length().toShort)
      w.append(hw)
    }

    w.putShort(obj.innerchain.length.toShort)
    obj.innerchain.map { h =>
      val hw = w.newWriter()
      HeaderSerializer.serialize(h, hw)
      w.putShort(hw.length().toShort)
      w.append(hw)
    }
  }

  override def parse(r: ScorexReader): KLS16Proof = {
    val m = r.getByte()
    val k = r.getByte()
    val i = r.getByte()
    val headSuffixLength = r.getShort()
    val headSuffix = HeaderSerializer.parse(r.newReader(r.getChunk(headSuffixLength)))

    @tailrec
    def parseSuffixes(acc: Seq[Header]): Seq[Header] = {
      if (acc.length == k) {
        acc.reverse
      } else {
        val l = r.getShort()
        val headerWithoutInterlinks = HeaderSerializer.parse(r.newReader(r.getChunk(l)))
        //TODO: fixme, What should happen if `acc` is empty?
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val interlinks = SpvAlgos.constructInterlinkVector(acc.head)
        parseSuffixes(headerWithoutInterlinks.copy(interlinks = interlinks) +: acc)
      }
    }

    val suffix = parseSuffixes(Seq(headSuffix))
    val interchainLength = r.getShort()

    val interchain = (0 until interchainLength) map { _ =>
      val l = r.getShort()
      HeaderSerializer.parse(r.newReader(r.getChunk(l)))
    }
    KLS16Proof(m, k, i, interchain, suffix)
  }
}

