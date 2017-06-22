package examples.spv

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.commons.SimpleBoxTransaction
import examples.spv.Constants._
import io.circe.Json
import io.circe.syntax._
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.Try

case class Header(parentId: BlockId,
                  interlinks: Seq[Array[Byte]],
                  stateRoot: Array[Byte],
                  transactionsRoot: Array[Byte],
                  timestamp: Block.Timestamp,
                  nonce: Int) extends PersistentNodeViewModifier[PublicKey25519Proposition, SimpleBoxTransaction] {
  override def transactions: Option[Seq[SimpleBoxTransaction]] = None

  override val modifierTypeId: ModifierTypeId = 100: Byte

  override lazy val id: ModifierId = hashfn(bytes)

  lazy val realDifficulty: BigInt = SpvAlgos.blockIdDifficulty(id)

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "innerchainLinks" -> interlinks.map(l => Base58.encode(l).asJson).asJson,
    "transactionsRoot" -> Base58.encode(transactionsRoot).asJson,
    "stateRoot" -> Base58.encode(stateRoot).asJson,
    "parentId" -> Base58.encode(parentId).asJson,
    "timestamp" -> timestamp.asJson,
    "nonce" -> nonce.asJson
  ).asJson


  override def toString: String = s"Header(${json.noSpaces})"

  override type M = Header

  override def serializer: Serializer[Header] = HeaderSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Header => id sameElements that.id
    case _ => false
  }
}

object HeaderSerializer extends Serializer[Header] {
  override def toBytes(h: Header): Array[Byte] = {
    def interlinkBytes(links: Seq[Array[Byte]], acc: Array[Byte]): Array[Byte] = {
      if (links.isEmpty) {
        acc
      } else {
        val headLink: Array[Byte] = links.head
        val repeating: Byte = links.count(_ sameElements headLink).toByte
        interlinkBytes(links.drop(repeating), Bytes.concat(acc, Array(repeating), headLink))
      }
    }
    Bytes.concat(bytesWithoutInterlinks(h), interlinkBytes(h.interlinks, Array[Byte]()))
  }

  val BytesWithoutInterlinksLength = 108

  def bytesWithoutInterlinks(h: Header): Array[Byte] = {
    Bytes.concat(h.parentId, h.transactionsRoot, h.stateRoot, Longs.toByteArray(h.timestamp), Ints.toByteArray(h.nonce))
  }

  override def parseBytes(bytes: Array[Byte]): Try[Header] = Try {
    val parentId = bytes.slice(0, 32)
    val transactionsRoot = bytes.slice(32, 64)
    val stateRoot = bytes.slice(64, 96)
    val timestamp = Longs.fromByteArray(bytes.slice(96, 104))
    val nonce = Ints.fromByteArray(bytes.slice(104, 108))

    @tailrec
    def parseInnerchainLinks(index: Int, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = if (bytes.length > index) {
      val repeatN: Int = bytes.slice(index, index + 1).head
      val link: Array[Byte] = bytes.slice(index + 1, index + 33)
      val links: Seq[Array[Byte]] = Array.fill(repeatN)(link)
      parseInnerchainLinks(index + 33, acc ++ links)
    } else {
      acc
    }
    val innerchainLinks = parseInnerchainLinks(108, Seq())

    Header(parentId, innerchainLinks, stateRoot, transactionsRoot, timestamp, nonce)
  }
}