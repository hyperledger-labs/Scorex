package examples.spv

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.spv.Constants._
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexEncoding
import scorex.core.{ModifierId, ModifierTypeId, PersistentNodeViewModifier}

import scala.annotation.tailrec
import scala.util.Try

case class Header(parentId: BlockId,
                  interlinks: Seq[ModifierId],
                  stateRoot: Array[Byte],
                  transactionsRoot: Array[Byte],
                  timestamp: Block.Timestamp,
                  nonce: Int) extends PersistentNodeViewModifier {

  override val modifierTypeId: ModifierTypeId = ModifierTypeId @@ 100.toByte

  override lazy val id: ModifierId = ModifierId @@ new String(hashfn(bytes))

  lazy val realDifficulty: BigInt = SpvAlgos.blockIdDifficulty(id)

  override def toString: String = s"Header(${this.asJson.noSpaces})"

  override type M = Header

  override def serializer: Serializer[Header] = HeaderSerializer
}

object Header extends ScorexEncoding {
  implicit val headerEncoder: Encoder[Header] = (h: Header) =>
    Map(
      "id" -> encoder.encode(h.id).asJson,
      "innerchainLinks" -> h.interlinks.map(l => encoder.encode(l).asJson).asJson,
      "transactionsRoot" -> encoder.encode(h.transactionsRoot).asJson,
      "stateRoot" -> encoder.encode(h.stateRoot).asJson,
      "parentId" -> encoder.encode(h.parentId).asJson,
      "timestamp" -> h.timestamp.asJson,
      "nonce" -> h.nonce.asJson
    ).asJson
}

object HeaderSerializer extends Serializer[Header] {
  override def toBytes(h: Header): Array[Byte] = {
    @tailrec
    def interlinkBytes(links: Seq[ModifierId], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        // `links` is not empty, it is safe to call head
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val headLink: ModifierId = links.head
        val repeating: Byte = links.count(_ == headLink).toByte
        interlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), headLink.getBytes("UTF-8")))
      }
    }
    Bytes.concat(bytesWithoutInterlinks(h), interlinkBytes(h.interlinks, Array[Byte]()))
  }

  val BytesWithoutInterlinksLength: Int = 108

  def bytesWithoutInterlinks(h: Header): Array[Byte] = {
    Bytes.concat(h.parentId.getBytes("UTF-8"), h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp),
      Ints.toByteArray(h.nonce))
  }

  override def parseBytes(bytes: Array[Byte]): Try[Header] = Try {
    val parentId = ModifierId @@ new String(bytes.slice(0, 32))
    val transactionsRoot = bytes.slice(32, 64)
    val stateRoot = bytes.slice(64, 96)
    val timestamp = Longs.fromByteArray(bytes.slice(96, 104))
    val nonce = Ints.fromByteArray(bytes.slice(104, 108))

    @tailrec
    def parseInnerchainLinks(index: Int, acc: Seq[ModifierId]): Seq[ModifierId] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: ModifierId = ModifierId @@ new String(bytes.slice(index + 1, index + 33))
      val links: Seq[ModifierId] = Array.fill(repeatN)(link)
      parseInnerchainLinks(index + 33, acc ++ links)
    } else {
      acc
    }
    val innerchainLinks = parseInnerchainLinks(108, Seq())

    Header(parentId, innerchainLinks, stateRoot, transactionsRoot, timestamp, nonce)
  }
}