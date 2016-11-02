package scorex.core.transaction.wallet

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.core.NodeViewModifier
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.core.transaction.state.Secret

import scala.util.Try

case class WalletBox[P <: Proposition, B <: Box[P]](box: B, transactionId: Array[Byte], createdAt: Long)

object WalletBox {
  def parse[P <: Proposition, B <: Box[P]](bytes: Array[Byte])(boxDeserializer: Array[Byte] => Try[B]): Try[WalletBox[P, B]] = Try {
    val txId = bytes.slice(0, NodeViewModifier.ModifierIdSize)
    val createdAt = Longs.fromByteArray(
      bytes.slice(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize + 8))
    val boxB = bytes.slice(NodeViewModifier.ModifierIdSize + 8, bytes.length)
    boxDeserializer(boxB).map(box => WalletBox[P, B](box, txId, createdAt))
  }.flatten

  def bytes[P <: Proposition, B <: Box[P]](box: WalletBox[P, B]): Array[Byte] = {
    Bytes.concat(box.transactionId, Longs.toByteArray(box.createdAt), box.box.bytes)
  }
}

case class WalletTransaction[P <: Proposition, TX <: Transaction[P]](proposition: P,
                                                                     tx: TX,
                                                                     blockId: Option[NodeViewModifier.ModifierId],
                                                                     createdAt: Long)

//todo: for Dmitry
object WalletTransaction {
  def parse[P <: Proposition, TX <: Transaction[P]](bytes: Array[Byte])
                                                   (propDeserializer: Array[Byte] => Try[P],
                                                    txDeserializer: Array[Byte] => Try[TX]
                                                   ): Try[WalletTransaction[P, TX]] = Try {
    val propLength = Ints.fromByteArray(bytes.slice(0, 4))
    var pos = 4
    val propTry = propDeserializer(bytes.slice(pos, pos + propLength))
    pos = pos + propLength

    val txLength = Ints.fromByteArray(bytes.slice(pos, pos + 4))
    val txTry = txDeserializer(bytes.slice(pos, pos + txLength))
    pos = pos + txLength

    val blockIdOpt: Option[NodeViewModifier.ModifierId] =
      if (bytes.slice(pos, pos + 1).head == 0) {
        pos = pos + 1
        None
      }
      else {
        val o = Some(bytes.slice(pos + 1, pos + 1 + NodeViewModifier.ModifierIdSize))
        pos = pos + 1 + NodeViewModifier.ModifierIdSize
        o
      }

    val createdAt = Longs.fromByteArray(bytes.slice(pos, pos + 8))


    WalletTransaction[P, TX](propTry.get, txTry.get,blockIdOpt, createdAt)
  }

  def bytes[P <: Proposition, TX <: Transaction[P]](wt: WalletTransaction[P, TX]): Array[Byte] = {
    val propBytes = wt.proposition.bytes
    val txBytes = wt.tx.bytes
    val bIdBytes = wt.blockId.map(id => Array(1: Byte) ++ id).getOrElse(Array(0: Byte))

    Ints.toByteArray(propBytes.length) ++
      propBytes ++
      Ints.toByteArray(txBytes.length) ++
      txBytes ++
      bIdBytes ++
      Longs.toByteArray(wt.createdAt)
  }
}


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait Wallet[P <: Proposition, TX <: Transaction[P], W <: Wallet[P, TX, W]]
  extends Vault[P, TX, W] {

  type S <: Secret
  type PI <: ProofOfKnowledgeProposition[S]

  def generateNewSecret(): W

  def historyTransactions: Seq[WalletTransaction[P, TX]]

  def boxes(): Seq[WalletBox[P, _ <: Box[P]]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}