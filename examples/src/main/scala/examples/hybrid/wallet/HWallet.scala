package examples.hybrid.wallet

import java.io.File

import com.google.common.primitives.Ints
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{HybridBlock, PosBlock}
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletBoxSerializer, WalletTransaction}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.utils.Random

import scala.collection.JavaConversions._
import scala.util.Try


case class HWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                   boxStore: LSMStore,
                   metaDb: DB)
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, HybridBlock, HWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  lazy val seedAppendix = metaDb.atomicInteger("seedAppendix", 0).createOrOpen()
  lazy val secretsMap = metaDb.treeMap("secrets", Serializer.BYTE_ARRAY, Serializer.BYTE_ARRAY).createOrOpen()

  lazy val boxIds = metaDb.treeSet("ids", Serializer.BYTE_ARRAY).createOrOpen()

  private lazy val walletBoxSerializer =
    new WalletBoxSerializer[PublicKey25519Proposition, PublicKey25519NoncedBox](PublicKey25519NoncedBoxSerializer)


  override def generateNewSecret(): HWallet = {
    val apdx = seedAppendix.incrementAndGet()
    val s = FastCryptographicHash(seed ++ Ints.toByteArray(apdx))
    val (priv, _) = PrivateKey25519Companion.generateKeys(s)
    secretsMap.put(priv.publicKeyBytes, priv.privKeyBytes)
    metaDb.commit()
    HWallet(seed, boxStore, metaDb)
  }

  //not implemented intentionally for now
  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, SimpleBoxTransaction]] = ???

  override def boxes(): Seq[WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    boxIds
      .flatMap(id => boxStore.get(ByteArrayWrapper(id)))
      .map(_.data)
      .map(ba => walletBoxSerializer.parseBytes(ba))
      .map(_.get)
      .toSeq
  }

  override def publicKeys: Set[PublicKey25519Proposition] =
    secretsMap.keyIterator().map(ba => PublicKey25519Proposition(ba)).toSet

  override def secrets: Set[PrivateKey25519] =
    secretsMap.iterator.map { case (pubK, privK) => PrivateKey25519(privK, pubK) }.toSet

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    Option(secretsMap.get(publicImage.bytes)).map(priv => PrivateKey25519(priv, publicImage.bytes))

  //we do not process offchain (e.g. by adding them to the wallet)
  override def scanOffchain(tx: SimpleBoxTransaction): HWallet = this

  override def scanOffchain(txs: Seq[SimpleBoxTransaction]): HWallet = this

  override def scanPersistent(modifier: HybridBlock): HWallet = {
    log.debug(s"Applying modifier to wallet: ${Base58.encode(modifier.id)}")

    val txs = modifier.transactions.getOrElse(Seq())

    val newBoxes = txs.flatMap { tx =>
      tx.newBoxes.map { box =>
        boxIds.add(box.id)
        val wb = WalletBox[PublicKey25519Proposition, PublicKey25519NoncedBox](box, tx.id,
          tx.timestamp)(PublicKey25519NoncedBoxSerializer)
        ByteArrayWrapper(box.id) -> ByteArrayWrapper(wb.bytes)
      }
    }
    val boxIdsToRemove = txs.flatMap(_.boxIdsToOpen).map(ByteArrayWrapper.apply)
    boxStore.update(ByteArrayWrapper(modifier.id), boxIdsToRemove, newBoxes)

    metaDb.commit()
    HWallet(seed, boxStore, metaDb)
  }

  override def rollback(to: VersionTag): Try[HWallet] = Try {
    if (boxStore.lastVersionID.exists(_.data sameElements to)) {
      this
    } else {
      log.debug(s"Rolling back wallet to: ${Base58.encode(to)}")
      boxStore.rollback(ByteArrayWrapper(to))
      HWallet(seed, boxStore, metaDb)
    }
  }

  override type NVCT = this.type

}

object HWallet {

  def readOrGenerate(settings: Settings, seed: String, appendix: String): HWallet = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    new File(dataDir).mkdirs()

    val wFile = new File(s"$dataDir/walletboxes")
    wFile.mkdirs()
    val boxesStorage = new LSMStore(wFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        boxesStorage.close()
      }
    })

    val mFile = new File(s"$dataDir/walletmeta" + appendix)

    lazy val metaDb =
      DBMaker.fileDB(mFile)
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    HWallet(Base58.decode(seed).get, boxesStorage, metaDb)
  }

  def readOrGenerate(settings: Settings, appendix: String): HWallet = {
    readOrGenerate(settings, Base58.encode(settings.walletSeed), appendix)
  }

  def readOrGenerate(settings: Settings, seed: String, appendix: String, accounts: Int): HWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings, seed, appendix)) { case (w, _) =>
      w.generateNewSecret()
    }

  def readOrGenerate(settings: Settings, appendix: String, accounts: Int): HWallet =
    (1 to accounts).foldLeft(readOrGenerate(settings, appendix)) { case (w, _) =>
      w.generateNewSecret()
    }

  def exists(settings: Settings): Boolean = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    new File(s"$dataDir/walletmeta").exists()
  }

  def readOrGenerate(settings: Settings): HWallet =
    readOrGenerate(settings, "", 10)

  //wallet with applied initialBlocks
  def genesisWallet(settings: Settings, initialBlocks: Seq[HybridBlock]): HWallet = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (a, b) =>
      a.scanPersistent(b)
    }
  }
}