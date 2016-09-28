package scorex.core.transaction.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.{DBMaker, HTreeMap}
import org.mapdb.serializer.SerializerByteArray
import scorex.core.{NodeViewComponent, NodeViewComponentCompanion}
import scorex.crypto.encode.Base58
import scorex.core.crypto.hash.SecureCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.{NodeViewModifier, Transaction}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion, Secret}
import scorex.utils.randomBytes

import scala.collection.JavaConversions._
import scala.util.Try

case class WalletBox[P <: Proposition](box: Box[P], transactionId: Array[Byte], createdAt: Int, destroyedAt: Option[Int])

case class WalletTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX, createdAt: Int)


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait Wallet[P <: Proposition, TX <: Transaction[P], W <: Wallet[P, TX, W]] extends NodeViewComponent {
  type VersionTag = NodeViewModifier.ModifierId

  type S <: Secret
  type PI <: ProofOfKnowledgeProposition[S]

  def generateNewSecret(): W

  //todo: or Try[Wallet[P, TX]] ?
  def scan(tx: TX, offchain: Boolean): W

  def bulkScan(txs: Seq[TX], offchain: Boolean): W

  def historyTransactions: Seq[WalletTransaction[P, TX]]

  def historyBoxes: Seq[WalletBox[P]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]

  def rollback(to: VersionTag): Try[W]
}


