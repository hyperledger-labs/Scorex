package scorex.core.transaction.wallet

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret
import scorex.core.utils.ScorexEncoding
import scorex.core.{ModifierId, NodeViewModifier, PersistentNodeViewModifier}

import scala.util.Try

case class WalletBox[P <: Proposition, B <: Box[P]](box: B)(subclassDeser: Serializer[B]) extends BytesSerializable
  with ScorexEncoding {

  override type M = WalletBox[P, B]

  override def serializer: Serializer[WalletBox[P, B]] = new WalletBoxSerializer(subclassDeser)
}


class WalletBoxSerializer[P <: Proposition, B <: Box[P]](subclassDeser: Serializer[B]) extends Serializer[WalletBox[P, B]] {
  override def toBytes(box: WalletBox[P, B]): Array[Byte] = box.box.bytes

  override def parseBytes(bytes: Array[Byte]): Try[WalletBox[P, B]] = Try {
    val box: B = subclassDeser.parseBytes(bytes).get
    WalletBox[P, B](box)(subclassDeser)
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

  //TODO Add Option[Seed] parameter, use provided seed it it exists
  def generateNewSecret(): W

  def historyTransactions: Seq[BoxWalletTransaction[P, TX]]

  def boxes(): Seq[WalletBox[P, _ <: Box[P]]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}