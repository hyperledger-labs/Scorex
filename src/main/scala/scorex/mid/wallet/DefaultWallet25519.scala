package scorex.mid.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import scorex.core.{PersistentNodeViewModifier, VersionTag}
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{PrivateKey, PublicKey}

import scala.collection.mutable
import scala.util.Try


//todo: finish
//todo: HKDF
// todo: encryption
//todo: persistence
case class DefaultWallet25519[TX <: Transaction[PublicKey25519Proposition], PMOD <: PersistentNodeViewModifier]
(settings: Settings)
  extends Wallet[PublicKey25519Proposition, TX, PMOD, DefaultWallet25519[TX, PMOD]] {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  val walletFileOpt: Option[File] = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  val password: String = settings.walletPassword
  val seed: Array[Byte] = settings.walletSeed


  private def lastNonce = 0

  private lazy val dbSecrets = mutable.Map[PublicKey, PrivateKey]()

  override def generateNewSecret(): DefaultWallet25519[TX, PMOD] = {
    val nonce = lastNonce + 1
    val randomSeed = Blake2b256(Bytes.concat(Ints.toByteArray(nonce), seed))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(randomSeed)

    dbSecrets.put(pub.pubKeyBytes, priv.privKeyBytes)
    new DefaultWallet25519[TX, PMOD](settings)
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, _ <: PublicKeyNoncedBox[PublicKey25519Proposition]]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] =
    dbSecrets.keySet.map(PublicKey25519Proposition.apply).toSet

  //todo: protection?
  override def secrets: Set[PrivateKey25519] = dbSecrets.map(e => PrivateKey25519(e._2, e._1)).toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    dbSecrets.get(publicImage.pubKeyBytes)
      .map(privBytes => PrivateKey25519(privBytes, publicImage.pubKeyBytes))

  override type NVCT = DefaultWallet25519[TX, PMOD]

  override def scanPersistent(modifier: PMOD): DefaultWallet25519[TX, PMOD] = ???

  override def scanOffchain(tx: TX): DefaultWallet25519[TX, PMOD] = ???

  override def scanOffchain(txs: Seq[TX]): DefaultWallet25519[TX, PMOD] = ???

  def rollback(to: VersionTag): Try[DefaultWallet25519[TX, PMOD]] = ??? //todo: fix
}
