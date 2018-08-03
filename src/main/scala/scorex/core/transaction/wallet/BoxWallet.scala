package scorex.core.transaction.wallet

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret
import scorex.core.utils.ScorexEncoding
import scorex.core._
import scorex.util._

import scala.util.Try

/**
  * TODO WalletBox is not used in Scorex and should be moved to `mid` layer.
  * It may be used in systems where a box does not contain a link to a corresponding transaction,
  * e.g. could be useful for developments of the Twinscoin protocol and wallet.
  *
  */
case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: ModifierId, createdAt: Long)
                                                   (subclassDeser: Serializer[B]) extends BytesSerializable
  with ScorexEncoding {

  override type M = WalletBox[P, B]

  override def serializer: Serializer[WalletBox[P, B]] = new WalletBoxSerializer(subclassDeser)

  override def toString: String = s"WalletBox($box, ${encoder.encode(transactionId)}, $createdAt)"
}


class WalletBoxSerializer[P <: Proposition, B <: Box[P]](subclassDeser: Serializer[B]) extends Serializer[WalletBox[P, B]] {
  override def toBytes(box: WalletBox[P, B]): Array[Byte] = {
    Bytes.concat(idToBytes(box.transactionId), Longs.toByteArray(box.createdAt), box.box.bytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox[P, B]] = Try {
    val txId = bytesToId(bytes.slice(0, NodeViewModifier.ModifierIdSize))
    val createdAt = Longs.fromByteArray(
      bytes.slice(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize + 8))
    val boxB = bytes.slice(NodeViewModifier.ModifierIdSize + 8, bytes.length)
    val box: B = subclassDeser.parseBytes(boxB).get
    WalletBox[P, B](box, txId, createdAt)(subclassDeser)
  }
}

case class BoxWalletTransaction[P <: Proposition, TX <: Transaction](proposition: P,
                                                                     tx: TX,
                                                                     blockId: Option[ModifierId],
                                                                     createdAt: Long)


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait BoxWallet[P <: Proposition, TX <: Transaction, PMOD <: PersistentNodeViewModifier, W <: BoxWallet[P, TX, PMOD, W]]
  extends Vault[TX, PMOD, W] {
  self: W =>

  type S <: Secret
  type PI <: ProofOfKnowledgeProposition[S]

  def generateNewSecret(): W

  def historyTransactions: Seq[BoxWalletTransaction[P, TX]]

  def boxes(): Seq[WalletBox[P, _ <: Box[P]]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}