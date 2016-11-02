package examples.hybrid.wallet

import com.google.common.primitives.Ints
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.LSMStore
import org.mapdb.{DB, Serializer}
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.Transaction._
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.utils.Random

import scala.util.Try


case class HWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                   txStore: LSMStore,
                   txIndexes: LSMStore,
                   boxStore: LSMStore,
                   boxIndexes: LSMStore,
                   metaDb: DB
                  )
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, HWallet]  {
  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  val chainTransactions: Map[TransactionId, SimpleBoxTransaction] = Map()
  val offchainTransactions: Map[TransactionId, SimpleBoxTransaction] = Map()
  val currentBalance: Long = 0

  lazy val seedAppendix = metaDb.atomicInteger("seedAppendix", 0).createOrOpen()
  lazy val secretsMap = metaDb.treeMap("secrets", Serializer.BYTE_ARRAY, Serializer.BYTE_ARRAY).createOrOpen()

  override def generateNewSecret(): HWallet = {
    val apdx = seedAppendix.incrementAndGet()
    val s = FastCryptographicHash(seed ++ Ints.toByteArray(apdx))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(s)
    secretsMap.put(pub.bytes, priv.bytes)
    metaDb.commit()
    HWallet(seed, txStore, txIndexes, boxStore, boxIndexes, metaDb)
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleBoxTransaction]] = {
    ???
  }

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = ???

  override def publicKeys: Set[PublicKey25519Proposition] = ???

  override def secrets: Set[PrivateKey25519] = ???

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(secretsMap.get(publicImage.bytes)).map(priv => PrivateKey25519(priv, publicImage.bytes))

  override def scan(tx: SimpleBoxTransaction, offchain: Boolean): HWallet = {
    val newBoxes = tx.newBoxes
    ???
  }

  override def bulkScan(txs: Seq[SimpleBoxTransaction], offchain: Boolean): HWallet = ???

  override def rollback(to: VersionTag): Try[HWallet] = {
    ???
  }

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}

object HWallet {
  def emptyWallet: HWallet = ??? //HWallet()
}