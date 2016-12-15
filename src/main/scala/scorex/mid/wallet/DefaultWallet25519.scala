package scorex.mid.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.serializer.SerializerByteArray
import org.mapdb.{DBMaker, HTreeMap}
import scorex.core.PersistentNodeViewModifier
import scorex.core.crypto.hash.DoubleCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}

import scala.collection.JavaConverters._
import scala.util.Try


//todo: HKDF
// todo: encryption
case class DefaultWallet25519[TX <: Transaction[PublicKey25519Proposition],
PMOD <: PersistentNodeViewModifier[PublicKey25519Proposition, TX]]
(settings: Settings)
  extends Wallet[PublicKey25519Proposition, TX, PMOD, DefaultWallet25519[TX, PMOD]] {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  val walletFileOpt: Option[File] = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  val password: String = settings.walletPassword
  val seed: Array[Byte] = settings.walletSeed

  val db = DBMaker
    .fileDB("wallet.dat")
    .make()

  private val dbSeed = db.atomicString("seed").createOrOpen()

  private def lastNonce = db.atomicInteger("nonce").createOrOpen()

  private lazy val dbSecrets: HTreeMap[Array[Byte], Array[Byte]] =
    db.hashMap("secrets", new SerializerByteArray, new SerializerByteArray).createOrOpen()

  override def generateNewSecret(): DefaultWallet25519[TX, PMOD] = {
    val nonce = lastNonce.incrementAndGet()
    val randomSeed = DoubleCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(randomSeed)

    dbSecrets.put(pub.pubKeyBytes, priv.privKeyBytes)
    db.commit()
    db.close()
    new DefaultWallet25519[TX, PMOD](settings)
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, _ <: PublicKeyNoncedBox[PublicKey25519Proposition]]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] =
    dbSecrets.getKeys.asScala.map(PublicKey25519Proposition.apply).toSet

  //todo: protection?
  override def secrets: Set[PrivateKey25519] =
  dbSecrets.getEntries.asScala.map(e => PrivateKey25519(e.getValue, e.getKey)).toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(dbSecrets.get(publicImage))
      .map(privBytes => PrivateKey25519(privBytes, publicImage.pubKeyBytes))

  override type NVCT = DefaultWallet25519[TX, PMOD]

  override def scanPersistent(modifier: PMOD): DefaultWallet25519[TX, PMOD] = ???

  override def scanOffchain(tx: TX): DefaultWallet25519[TX, PMOD] = ???

  override def scanOffchain(txs: Seq[TX]): DefaultWallet25519[TX, PMOD] = ???

  def rollback(to: VersionTag): Try[DefaultWallet25519[TX, PMOD]] = ??? //todo: fix
}
