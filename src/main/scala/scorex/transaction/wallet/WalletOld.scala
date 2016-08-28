package scorex.transaction.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.h2.mvstore.{MVMap, MVStore}
import org.mapdb.{DBMaker, HTreeMap}
import org.mapdb.serializer.SerializerByteArray
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.settings.Settings
import scorex.transaction.Transaction
import scorex.transaction.box.Box
import scorex.transaction.box.proposition.{Proposition, PublicImage, PublicKey25519Proposition}
import scorex.transaction.state.{PrivateKey25519, PrivateKey25519Companion, PublicKey25519, Secret}
import scorex.utils.{ScorexLogging, randomBytes}

import scala.collection.concurrent.TrieMap
import scala.collection.JavaConversions._

case class WalletBox[P <: Proposition](box: Box[P], transactionId: Array[Byte], createdAt: Int, destroyedAt: Option[Int])

case class WalletTransaction[P <: Proposition, TX <: Transaction[P, TX]](tx: TX, createdAt: Int)


/**
  * Abstract interface for a wallet
  *
  * @tparam P
  * @tparam TX
  */
trait Wallet[P <: Proposition, TX <: Transaction[P, TX]] {
  type S <: Secret
  type PI <: PublicImage[S]

  def generateNewSecret(): Wallet[P, TX]

  def scan(tx: Transaction[P, _ <: Transaction[P, _]]): Wallet[P, TX]

  def historyTransactions: Seq[WalletTransaction[P, TX]]

  def historyBoxes: Seq[WalletBox[P]]

  def publicKeys: Set[PI]

  //todo: protection?
  def secrets: Set[S]

  def secretByPublicImage(publicImage: PI): Option[S]
}


//todo: HKDF
// todo: encryption
case class DefaultWallet25519[TX <: Transaction[PublicKey25519Proposition, TX]]
(settings: Settings)
  extends Wallet[PublicKey25519Proposition, TX] {

  override type S = PrivateKey25519
  override type PI = PublicKey25519

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

  override def generateNewSecret(): Wallet[PublicKey25519Proposition, TX] = {
    val nonce = lastNonce.incrementAndGet()
    val randomSeed = SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(randomSeed)

    dbSecrets.put(pub.publicKeyBytes, priv.privKeyBytes)
    db.commit()
    db.close()
    new DefaultWallet25519[TX](settings)
  }

  override def scan(tx: Transaction[PublicKey25519Proposition, _ <: Transaction[PublicKey25519Proposition, _]]): Wallet[PublicKey25519Proposition, TX] = ???

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, TX]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = ???

  override def publicKeys: Set[PublicKey25519] =
    dbSecrets.getKeys.map(PublicKey25519.apply).toSet

  //todo: protection?
  override def secrets: Set[PrivateKey25519] =
    dbSecrets.getEntries.map(e => PrivateKey25519(e.getValue, e.getKey)).toSet

  override def secretByPublicImage(publicImage: PublicKey25519): Option[PrivateKey25519] =
    Option(dbSecrets.get(publicImage))
      .map(privBytes => PrivateKey25519(privBytes, publicImage.publicKeyBytes))
}