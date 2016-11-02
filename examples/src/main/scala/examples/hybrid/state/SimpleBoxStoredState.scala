package examples.hybrid.state

import java.io.File

import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.mapdb.{DB, DBMaker, Serializer}
import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState

import scala.util.{Success, Try}

//todo: alter to have coinbase
object PowChanges extends StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set())

case class SimpleBoxStoredState(store: LSMStore, metaDb: DB, override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    HybridPersistentNodeViewModifier,
    SimpleBoxStoredState] {

  override type NVCT = SimpleBoxStoredState
  type HPMOD = HybridPersistentNodeViewModifier

  //blockId(state version) -> dbversion index, as IODB uses long int version
  lazy val dbVersions = metaDb.hashMap("vidx", Serializer.BYTE_ARRAY, Serializer.LONG).createOrOpen()

  private def dbVersion(ver: VersionTag): Long = dbVersions.get(ver)

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = Try {
    assert(tx.valid)
  }

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    Option(store.get(ByteArrayWrapper(boxId)))
      .map(_.data)
      .map(PublicKey25519NoncedBox.parseBytes)
      .flatMap(_.toOption)


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
                            newVersion: VersionTag): Try[SimpleBoxStoredState] = Try {
    val newDbVersion = store.lastVersion + 1
    dbVersions.put(newVersion, newDbVersion)
    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    store.update(newDbVersion, boxIdsToRemove, boxesToAdd)
    metaDb.commit()
    SimpleBoxStoredState(store, metaDb, newVersion)
  }

  override def rollbackTo(version: VersionTag): Try[SimpleBoxStoredState] = Try {
    store.rollback(dbVersion(version))
    new SimpleBoxStoredState(store, metaDb, version)
  }

  override def companion: NodeViewComponentCompanion = ???
}

object SimpleBoxStoredState {
  def emptyState(settings: Settings): SimpleBoxStoredState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    val mFile = new File(s"$dataDir/statemeta")

    val metaDb =
      DBMaker.fileDB(mFile)
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .make()

    SimpleBoxStoredState(stateStorage, metaDb, Array.emptyByteArray)
  }
}