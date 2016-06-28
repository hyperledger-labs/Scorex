package scorex.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash
import scorex.settings.Settings
import scorex.transaction.TransactionModule
import scorex.transaction.box.{Proposition, AddressableProposition}
import scorex.transaction.state.SecretHolderGenerator
import scorex.utils.{ScorexLogging, randomBytes}

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap

//todo: add accs txs?
class Wallet[P <: Proposition,
AP <: P with AddressableProposition,
TM <: TransactionModule[P, _, _]](settings: Settings,
                                       generator: SecretHolderGenerator[TM#SH])
  extends ScorexLogging {

  val walletFileOpt: Option[File] = settings.walletDirOpt.map(walletDir => new java.io.File(walletDir, "wallet.s.dat"))
  val password: String = settings.walletPassword
  val seedOpt: Option[Array[Byte]] = settings.walletSeed

  type SH = TM#SH

  private val NonceFieldName = "nonce"

  private val database: MVStore = walletFileOpt match {
    case Some(walletFile) =>
      //create parent folders then check their existence
      walletFile.getParentFile.mkdirs().ensuring(walletFile.getParentFile.exists())
      new MVStore.Builder().fileName(walletFile.getAbsolutePath).encryptionKey(password.toCharArray).compress().open()

    case None => new MVStore.Builder().open()
  }

  private val accountsPersistence: MVMap[Int, Array[Byte]] = database.openMap("secretHolders")
  private val seedPersistence: MVMap[String, Array[Byte]] = database.openMap("seed")
  private val noncePersistence: MVMap[String, Int] = database.openMap("nonce")

  if (Option(seedPersistence.get("seed")).isEmpty) {
    val seed = seedOpt.getOrElse {
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
    seedPersistence.put("seed", seed)
  }

  lazy val seed: Array[Byte] = seedPersistence.get("seed")

  private val accountsCache: TrieMap[String, SH] = {
    val shs = accountsPersistence
      .keys
      .map(k => accountsPersistence.get(k))
      .map(seed => generator.generateKeys(seed))

    TrieMap(shs.map(sh => sh.publicAddress -> sh).toSeq: _*)
  }

  def privateKeyAccounts(): Seq[SH] = accountsCache.values.toSeq

  def generateNewAccounts(howMany: Int): Seq[SH] = (1 to howMany).map(_ => generateNewAccount())

  def generateNewAccount(): SH = synchronized {
    val nonce = getAndIncrementNonce()

    val accountSeed = generateAccountSeed(seed, nonce)
    val secretHolder: SH = generator.generateKeys(accountSeed)

    accountsCache += secretHolder.publicAddress -> secretHolder
    accountsPersistence.put(accountsPersistence.lastKey() + 1, secretHolder.bytes)
    database.commit()

    log.info("Added account #" + nonce)
    secretHolder
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def deleteAccount(account: SH): Boolean = synchronized {
    //    val res = accountsPersistence.remove(account.seed)
    val res = accountsPersistence.keys.find { k =>
      if (accountsPersistence.get(k) sameElements account.bytes) {
        accountsPersistence.remove(k)
        true
      } else false
    }
    database.commit()
    accountsCache -= account.publicAddress
    res.isDefined
  }

  def privateKeyAccount(publicAddress: String): Option[SH] = accountsCache.get(publicAddress)

  def close(): Unit = if (!database.isClosed) {
    database.commit()
    database.close()
    accountsCache.clear()
  }

  def exists(): Boolean = walletFileOpt.map(_.exists()).getOrElse(true)

  def nonce(): Int = Option(noncePersistence.get(NonceFieldName)).getOrElse(0)

  def getAndIncrementNonce(): Int = synchronized {
    noncePersistence.put(NonceFieldName, nonce() + 1) - 1
  }
}