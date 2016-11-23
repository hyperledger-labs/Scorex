package examples.hybrid.state

import java.io.File

import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import examples.hybrid.mining.MiningSettings
import io.circe
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.NodeViewComponentCompanion
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.transaction.state.authenticated.BoxMinimalState

import scala.util.{Success, Try}

//todo: alter to have coinbase
object PowChanges extends StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set())

case class HBoxStoredState(store: LSMStore, metaDb: DB, override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    HybridPersistentNodeViewModifier,
    HBoxStoredState] {

  override type NVCT = HBoxStoredState
  type HPMOD = HybridPersistentNodeViewModifier

  //blockId(state version) -> dbversion index, as IODB uses long int version
  lazy val dbVersions = metaDb.hashMap("vidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  private def dbVersion(ver: VersionTag): Long = dbVersions.get(ver)

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = Try {
    assert(tx.valid)
  }

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] = {
    val res = Option(store.get(ByteArrayWrapper(boxId)))
      .map(_.data)
      .map(PublicKey25519NoncedBox.parseBytes)
      .flatMap(_.toOption)
    res
  }

  //there's no an easy way to know boxes associated with a proposition, without an additional index
  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def changes(mod: HPMOD):Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    mod match {
      case pb: PowBlock => Success(PowChanges)
      case ps: PosBlock =>
        Try {
          val initial = (Set(): Set[Array[Byte]], Set(): Set[PublicKey25519NoncedBox], 0L)

          val (toRemove: Set[Array[Byte]], toAdd: Set[PublicKey25519NoncedBox], reward) =
            ps.transactions.map(_.foldLeft(initial) { case ((sr, sa, f), tx) =>
              (sr ++ tx.boxIdsToOpen.toSet, sa ++ tx.newBoxes.toSet, f + tx.fee)
            }).getOrElse((Set(), Set(), 0L)) //no reward additional to tx fees

          //for PoS forger reward box, we use block Id as a nonce
          val forgerNonce = Longs.fromByteArray(ps.id.take(8))
          val forgerBox = PublicKey25519NoncedBox(ps.generator, forgerNonce, reward)
          StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAdd ++ Set(forgerBox))
        }
    }
  }

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[HBoxStoredState] = Try {
    val newDbVersion = store.lastVersion + 1
    dbVersions.put(newVersion, newDbVersion)
    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    store.update(newDbVersion, boxIdsToRemove, boxesToAdd)
    metaDb.commit()
    HBoxStoredState(store, metaDb, newVersion)
  }

  override def rollbackTo(version: VersionTag): Try[HBoxStoredState] = Try {
    store.rollback(dbVersion(version))
    new HBoxStoredState(store, metaDb, version)
  }

  override def companion: NodeViewComponentCompanion = ???
}

object HBoxStoredState {
  def readOrGenerate(settings: Settings): HBoxStoredState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stateStorage.close()
      }
    })

    val mFile = new File(s"$dataDir/statemeta")

    val metaDb =
      DBMaker.fileDB(mFile)
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    HBoxStoredState(stateStorage, metaDb, Array.emptyByteArray)
  }

  def genesisState(settings: Settings, initialBlock: PosBlock): HBoxStoredState =
    readOrGenerate(settings).applyModifier(initialBlock).get
}


//todo: convert to tests
object HStatePlayground extends App {
  val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")
  }

  val s0 = HBoxStoredState.readOrGenerate(settings)

  val b = PublicKey25519NoncedBox(PublicKey25519Proposition(Array.fill(32)(0:Byte)), 10000L, 500L)

  val c = StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set(b))

  val s1 = s0.applyChanges(c, Array.fill(32)(0: Byte)).get

  assert(s1.closedBox(b.id).isDefined)
}