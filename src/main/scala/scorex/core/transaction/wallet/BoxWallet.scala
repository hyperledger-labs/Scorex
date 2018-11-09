package scorex.core.transaction.wallet


import scorex.core.newserialization._
import scorex.core.{NodeViewModifier, PersistentNodeViewModifier}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret
import scorex.core.utils.ScorexEncoding
import scorex.util.{ModifierId, bytesToId, idToBytes}

/**
  * TODO WalletBox is not used in Scorex and should be moved to `mid` layer.
  * It may be used in systems where a box does not contain a link to a corresponding transaction,
  * e.g. could be useful for developments of the Twinscoin protocol and wallet.
  *
  */
case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: ModifierId, createdAt: Long)
  extends ScorexEncoding {

  override def toString: String = s"WalletBox($box, ${encoder.encodeId(transactionId)}, $createdAt)"
}

class WalletBoxSerializer[P <: Proposition, B <: Box[P]](subclassDeser: ScorexSerializer[B]) extends ScorexSerializer[WalletBox[P, B]] {

  override def serialize(box: WalletBox[P, B], w: ScorexWriter): Unit = {
    w.putBytes(idToBytes(box.transactionId))
    w.putLong(box.createdAt)
    subclassDeser.serialize(box.box, w)
  }

  override def parse(r: ScorexReader): WalletBox[P, B] = {
    val txId = bytesToId(r.getBytes(NodeViewModifier.ModifierIdSize))
    val createdAt = r.getLong()
    val box = subclassDeser.parse(r)
    WalletBox[P, B](box, txId, createdAt)
  }
}

case class BoxWalletTransaction[P <: Proposition, TX <: Transaction](proposition: P,
                                                                     tx: TX,
                                                                     blockId: Option[ModifierId],
                                                                     createdAt: Long)


/**
  * Abstract interface for a wallet
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
