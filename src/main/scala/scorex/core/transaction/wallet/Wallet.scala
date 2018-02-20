package scorex.core.transaction.wallet

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret
import scorex.core.{ModifierId, NodeViewModifier, PersistentNodeViewModifier}
import scorex.crypto.encode.Base58

import scala.util.Try

//TODO why do we need transactionId and createdAt
case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: Array[Byte], createdAt: Long)
                                                   (subclassDeser: Serializer[B]) extends BytesSerializable {
  override type M = WalletBox[P, B]

  override def serializer: Serializer[WalletBox[P, B]] = new WalletBoxSerializer(subclassDeser)

  override def toString: String = s"WalletBox($box, ${Base58.encode(transactionId)}, $createdAt)"
}


class WalletBoxSerializer[P <: Proposition, B <: Box[P]](subclassDeser: Serializer[B]) extends Serializer[WalletBox[P, B]] {
  override def toBytes(box: WalletBox[P, B]): Array[Byte] = {
    Bytes.concat(box.transactionId, Longs.toByteArray(box.createdAt), box.box.bytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox[P, B]] = Try {
    val txId = bytes.slice(0, NodeViewModifier.ModifierIdSize)
    val createdAt = Longs.fromByteArray(
      bytes.slice(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize + 8))
    val boxB = bytes.slice(NodeViewModifier.ModifierIdSize + 8, bytes.length)
    val box: B = subclassDeser.parseBytes(boxB).get
    WalletBox[P, B](box, txId, createdAt)(subclassDeser)
  }
}

case class WalletTransaction[P <: Proposition, TX <: Transaction[P]](proposition: P,
                                                                     tx: TX,
                                                                     blockId: Option[ModifierId],
                                                                     createdAt: Long)

object WalletTransaction {
  def parse[P <: Proposition, TX <: Transaction[P]](bytes: Array[Byte])
                                                   (propDeserializer: Array[Byte] => Try[P],
                                                    txDeserializer: Array[Byte] => Try[TX]
                                                   ): Try[WalletTransaction[P, TX]] = Try {
    val propLength = Ints.fromByteArray(bytes.slice(0, 4)) // scalastyle:ignore magic.number
    var pos = 4 // scalastyle:ignore magic.number
    val propTry = propDeserializer(bytes.slice(pos, pos + propLength))
    pos = pos + propLength

    val txLength = Ints.fromByteArray(bytes.slice(pos, pos + 4))
    val txTry = txDeserializer(bytes.slice(pos, pos + txLength))
    pos = pos + txLength

    val blockIdOpt: Option[ModifierId] =
      if (bytes.slice(pos, pos + 1).head == 0) {
        pos = pos + 1
        None
      }
      else {
        val o = ModifierId @@ Some(bytes.slice(pos + 1, pos + 1 + NodeViewModifier.ModifierIdSize))
        pos = pos + 1 + NodeViewModifier.ModifierIdSize
        o
      }

    val createdAt = Longs.fromByteArray(bytes.slice(pos, pos + 8))


    WalletTransaction[P, TX](propTry.get, txTry.get, blockIdOpt, createdAt)
  }

  def bytes[P <: Proposition, TX <: Transaction[P]](wt: WalletTransaction[P, TX]): Array[Byte] = {
    val propBytes = wt.proposition.bytes
    val txBytes = wt.tx.bytes
    val bIdBytes = wt.blockId.map(id => Array(1: Byte) ++ id).getOrElse(Array(0: Byte))

    Bytes.concat(Ints.toByteArray(propBytes.length), propBytes, Ints.toByteArray(txBytes.length), txBytes, bIdBytes,
      Longs.toByteArray(wt.createdAt))
  }
}


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait Wallet[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier, W <: Wallet[P, TX, PMOD, W]]
  extends Vault[P, TX, PMOD, W] {
  self: W =>

  type S <: Secret
  type PI <: ProofOfKnowledgeProposition[S]

  //TODO Add Option[Seed] parameter, use provided seed it it exists
  def generateNewSecret(): W

  def historyTransactions: Seq[WalletTransaction[P, TX]]

  def boxes(): Seq[WalletBox[P, _ <: Box[P]]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}
