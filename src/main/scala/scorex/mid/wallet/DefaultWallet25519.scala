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
import scorex.crypto.encode.Base58
import scorex.utils._
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
  val seedOpt: Option[Array[Byte]] = settings.walletSeed

  val db = DBMaker
    .fileDB("wallet.dat")
    .make()

  private val dbSeed = db.atomicString("seed").createOrOpen()

  lazy val seed: Array[Byte] = Option(dbSeed.get())
    .map(Base58.decode)
    .flatMap(_.toOption)
    .getOrElse {
      val s = seedOpt.getOrElse {
        val Attempts = 10
        val SeedSize = 64
        lazy val randomSeed = randomBytes(SeedSize)
        def readSeed(limit: Int = Attempts): Array[Byte] = {
          println("Please type your wallet seed or type Enter to generate random one")
          val typed = scala.io.StdIn.readLine()
          if (typed.isEmpty) {
            println(s"Your random generated seed is ${Base58.encode(randomSeed)}")
            randomSeed
          } else
            Base58.decode(typed).getOrElse {
              if (limit > 0) {
                println("Wallet seed should be correct Base58 encoded string.")
                readSeed(limit - 1)
              } else throw new Error("Sorry you have made too many incorrect seed guesses")
            }
        }
        readSeed()
      }
      dbSeed.set(Base58.encode(s))
      s
    }

  private def lastNonce = db.atomicInteger("nonce").createOrOpen()

  private lazy val dbSecrets: HTreeMap[Array[Byte], Array[Byte]] =
    db.hashMap("secrets", new SerializerByteArray, new SerializerByteArray).createOrOpen()

  override def generateNewSecret(seed: Array[Byte]): DefaultWallet25519[TX] = ???

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

  override def bulkScan(txs: Seq[TX], offchain: Boolean):  DefaultWallet25519[TX] = ???

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

  override def companion: NodeViewComponentCompanion = ???  //todo: fix

  def rollback(to: VersionTag) = ???  //todo: fix
}
