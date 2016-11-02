package examples.hybrid.wallet

import java.io.File

import com.google.common.primitives.Ints
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import examples.hybrid.state.SimpleBoxTransaction
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.NodeViewComponentCompanion
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.utils.Random

import scala.util.Try
import scala.collection.JavaConversions._


case class HWallet(seed: Array[Byte] = Random.randomBytes(PrivKeyLength),
                   boxStore: LSMStore,
                   metaDb: DB)
  extends Wallet[PublicKey25519Proposition, SimpleBoxTransaction, HybridPersistentNodeViewModifier, HWallet] {

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
    HWallet(seed, boxStore, metaDb)
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
        ByteArrayWrapper(box.id) -> ByteArrayWrapper(WalletBox(box, tx.id, tx.timestamp).bytes)
      }
    }
    val boxIdsToRemove = txs.flatMap(_.boxIdsToOpen).map(ByteArrayWrapper)
    val newVersion = boxStore.lastVersion + 1

    boxStore.update(newVersion, boxIdsToRemove, newBoxes)

    dbVersions.put(modifier.id, newVersion)

    metaDb.commit()
    HWallet(seed, boxStore, metaDb)
  }

  private def dbVersion(ver: VersionTag): Long = dbVersions.get(ver)

  override def rollback(to: VersionTag): Try[HWallet] = Try {
    boxStore.rollback(dbVersion(to))
    HWallet(seed, boxStore, metaDb)
  }

  override type NVCT = this.type

  override def companion: NodeViewComponentCompanion = ???
}

object HWallet {

  def emptyWallet(settings: Settings): HWallet = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    val wFile = new File(s"$dataDir/walletboxes")
    wFile.mkdirs()
    val boxesStorage = new LSMStore(wFile)

    val mFile = new File(s"$dataDir/walletmeta")
    mFile.mkdirs()

    val metaDb =
      DBMaker.fileDB(mFile)
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    HWallet(settings.walletSeed, boxesStorage, metaDb)
  }
}