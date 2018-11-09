package examples.spv

import examples.spv.Constants._
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.{ModifierTypeId, PersistentNodeViewModifier}
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.newserialization.{ByteStringWriter, ScorexReader, ScorexSerializer, ScorexWriter}
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.annotation.tailrec

case class Header(parentId: BlockId,
                  interlinks: Seq[ModifierId],
                  stateRoot: Array[Byte],
                  transactionsRoot: Array[Byte],
                  timestamp: Block.Timestamp,
                  nonce: Int) extends PersistentNodeViewModifier {

  override val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 100.toByte

  override lazy val id: ModifierId = bytesToId(hashfn(HeaderSerializer.toBytes(this)))

  lazy val realDifficulty: BigInt = SpvAlgos.blockIdDifficulty(id)

  override def toString: String = s"Header(${this.asJson.noSpaces})"
}

object Header extends ScorexEncoding {
  implicit val headerEncoder: Encoder[Header] = (h: Header) =>
    Map(
      "id" -> encoder.encodeId(h.id).asJson,
      "innerchainLinks" -> h.interlinks.map(l => encoder.encodeId(l).asJson).asJson,
      "transactionsRoot" -> encoder.encode(h.transactionsRoot).asJson,
      "stateRoot" -> encoder.encode(h.stateRoot).asJson,
      "parentId" -> encoder.encodeId(h.parentId).asJson,
      "timestamp" -> h.timestamp.asJson,
      "nonce" -> h.nonce.asJson
    ).asJson
}

object HeaderSerializer extends ScorexSerializer[Header] {

  override def serialize(h: Header, w: ScorexWriter): Unit = {
    serializeWithoutItrelinks(h, w)

    @tailrec
    def writeInterlinks(links: Seq[ModifierId]):Unit = {
      links.headOption match {
        case Some(headLink) =>
          val repeating: Version = links.count(_ == headLink).toByte
          w.put(repeating)
          w.putBytes(idToBytes(headLink))
          writeInterlinks(links.drop(repeating))
        case None =>
      }
    }

    writeInterlinks(h.interlinks)
  }

  def serializeWithoutItrelinks(h: Header, w: ScorexWriter): Unit = {
    w.putBytes(idToBytes(h.parentId))
    w.putBytes(h.transactionsRoot)
    w.putBytes(h.stateRoot)
    w.putLong(h.timestamp)
    w.putInt(h.nonce)
  }

  val BytesWithoutInterlinksLength: Int = 108

  def bytesWithoutInterlinks(h: Header): Array[Byte] = {
    val w = new ByteStringWriter
    serializeWithoutItrelinks(h, w)
    w.result().toArray
  }

  override def parse(r: ScorexReader): Header = {
    val parentId = bytesToId(r.getBytes(32))
    val transactionsRoot = r.getBytes(32)
    val stateRoot = r.getBytes(32)
    val timestamp = r.getLong()
    val nonce = r.getInt()

    @tailrec
    def parseInnerchainLinks(acc: Seq[ModifierId]): Seq[ModifierId] = {
      if (r.remaining > 0) {
        val repeatN: Int = r.getByte()
        val link: ModifierId = bytesToId(r.getBytes(32))
        val links: Seq[ModifierId] = Array.fill(repeatN)(link)
        parseInnerchainLinks(acc ++ links)
      } else {
        acc
      }
    }

    val innerchainLinks = parseInnerchainLinks(Seq())

    Header(parentId, innerchainLinks, stateRoot, transactionsRoot, timestamp, nonce)
  }
}
