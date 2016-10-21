package scorex.mid.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.mapdb.serializer.SerializerByteArray
import org.mapdb.{DBMaker, HTreeMap}
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.DoubleCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}

import scala.collection.JavaConversions._

/**
  * Created by kushti on 28.09.16.
  */
//todo: HKDF
// todo: encryption
case class DefaultWallet25519[TX <: Transaction[PublicKey25519Proposition]]
(settings: Settings)
  extends Wallet[PublicKey25519Proposition, TX, DefaultWallet25519[TX]] {

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

  override def generateNewSecret(): DefaultWallet25519[TX] = {
    val nonce = lastNonce.incrementAndGet()
    val randomSeed = DoubleCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(randomSeed)

    dbSecrets.put(pub.pubKeyBytes, priv.privKeyBytes)
    db.commit()
    db.close()
    new DefaultWallet25519[TX](settings)
  }

  override def scan(tx: TX, offchain: Boolean): DefaultWallet25519[TX] = ???

  override def bulkScan(txs: Seq[TX], offchain: Boolean): DefaultWallet25519[TX] = ???

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] =
    dbSecrets.getKeys.map(PublicKey25519Proposition.apply).toSet

  //todo: protection?
  override def secrets: Set[PrivateKey25519] =
    dbSecrets.getEntries.map(e => PrivateKey25519(e.getValue, e.getKey)).toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(dbSecrets.get(publicImage))
      .map(privBytes => PrivateKey25519(privBytes, publicImage.pubKeyBytes))

  override type NVCT = DefaultWallet25519[TX]

  override def companion: NodeViewComponentCompanion = ??? //todo: fix

  def rollback(to: VersionTag) = ??? //todo: fix
}
