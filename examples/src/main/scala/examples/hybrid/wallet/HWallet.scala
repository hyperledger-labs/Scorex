package examples.hybrid.wallet

import java.io.File

import com.google.common.primitives.Ints
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock}
import examples.hybrid.mining.MiningSettings
import examples.hybrid.state.SimpleBoxTransaction
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DBMaker, Serializer}
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.encode.Base58
import scorex.utils.Random

import scala.util.Try
import scala.collection.JavaConversions._


//todo: file for MapDB database passed in here, while in other class just a database
//todo: check and fix
case class HWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                   boxStore: LSMStore,
                   metaDbFile: File)
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HWallet] {

  lazy val metaDb =
    DBMaker.fileDB(metaDbFile)
      .fileMmapEnableIfSupported()
      .closeOnJvmShutdown()
      .make()

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  lazy val seedAppendix = metaDb.atomicInteger("seedAppendix", 0).createOrOpen()
  lazy val secretsMap = metaDb.treeMap("secrets", Serializer.BYTE_ARRAY, Serializer.BYTE_ARRAY).createOrOpen()

  lazy val dbVersions = metaDb.hashMap("versions", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  lazy val boxIds = metaDb.treeSet("ids", Serializer.BYTE_ARRAY).createOrOpen()

  override def generateNewSecret(): HWallet = {
    val apdx = seedAppendix.incrementAndGet()
    val s = FastCryptographicHash(seed ++ Ints.toByteArray(apdx))
    val (priv, pub) = PrivateKey25519Companion.generateKeys(s)
    secretsMap.put(pub.bytes, priv.bytes)
    metaDb.commit()
    metaDb.close()
    HWallet(seed, boxStore, metaDbFile)
  }

  //not implemented intentionally for now
  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleBoxTransaction]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    boxIds
      .flatMap(id => Option(boxStore.get(ByteArrayWrapper(id))))
      .map(_.data)
      .map(ba => WalletBox.parse[PublicKey25519Proposition, PublicKey25519NoncedBox](ba)(PublicKey25519NoncedBox.parseBytes))
      .map(_.get)
      .toSeq

  override def publicKeys: Set[PublicKey25519Proposition] =
    secretsMap.keyIterator().map(ba => PublicKey25519Proposition(ba)).toSet

  override def secrets: Set[PrivateKey25519] =
    secretsMap.iterator.map { case (pubK, privK) => PrivateKey25519(privK, pubK) }.toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(secretsMap.get(publicImage.bytes)).map(priv => PrivateKey25519(priv, publicImage.bytes))

  //we do not process offchain (e.g. by adding them to the wallet)
  override def scanOffchain(tx: SimpleBoxTransaction): HWallet = this

  override def scanOffchain(txs: Seq[SimpleBoxTransaction]): HWallet = this

  override def scanPersistent(modifier: HybridPersistentNodeViewModifier): HWallet = {
    val txs = modifier.transactions.getOrElse(Seq())

    val newBoxes = txs.flatMap { tx =>
      tx.newBoxes.map { box =>
        boxIds.add(box.id)
        println("box id: " + Base58.encode(box.id))
        val wb = WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, tx.id, tx.timestamp)
        ByteArrayWrapper(box.id) -> ByteArrayWrapper(wb.bytes)
      }
    }
    val boxIdsToRemove = txs.flatMap(_.boxIdsToOpen).map(ByteArrayWrapper)
    val newVersion = boxStore.lastVersion + 1

    boxStore.update(newVersion, boxIdsToRemove, newBoxes)

    dbVersions.put(modifier.id, newVersion)

    metaDb.commit()
    metaDb.close()
    HWallet(seed, boxStore, metaDbFile)
  }

  private def dbVersion(ver: VersionTag): Long = dbVersions.get(ver)

  override def rollback(to: VersionTag): Try[HWallet] = Try {
    boxStore.rollback(dbVersion(to))
    HWallet(seed, boxStore, metaDbFile)
  }

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}

object HWallet {

  def emptyWallet(settings: Settings, appendix:String): HWallet = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    new File(dataDir).mkdirs()

    val wFile = new File(s"$dataDir/walletboxes")
    wFile.mkdirs()
    val boxesStorage = new LSMStore(wFile)

    val mFile = new File(s"$dataDir/walletmeta" + appendix)

    HWallet(settings.walletSeed, boxesStorage, mFile)
  }

  def emptyWallet(settings: Settings, appendix:String, accounts: Int): HWallet =
    (1 to 10).foldLeft(emptyWallet(settings, appendix)) { case (w, _) =>
      w.generateNewSecret()
    }

  //wallet with 10 accounts and initial data processed
  def genesisWallet(settings: Settings, initialBlock: PosBlock): HWallet =
    emptyWallet(settings, "", 10).scanPersistent(initialBlock)
}


//todo: convert to a test
object WalletPlayground extends App {
  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

  val w = HWallet.emptyWallet(settings, "p").generateNewSecret().generateNewSecret()

  assert(w.secrets.size == 2)

  val fs = w.secrets.head
  val ss = w.secrets.tail.head

  val from = IndexedSeq(fs -> 500L, ss -> 1000L)
  val to = IndexedSeq(ss.publicImage -> 1500L)
  val fee = 500L
  val timestamp = System.currentTimeMillis()

  val tx = SimpleBoxTransaction(from, to, fee, timestamp)

  val za = Array.fill(32)(0: Byte)

  val pb = PosBlock(za, System.currentTimeMillis(), Seq(tx), fs.publicImage, Signature25519(za))

  println(w.scanPersistent(pb).boxes().head)
}