package examples.hybrid.state

import java.io.File

import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.HybridPersistentNodeViewModifier
import io.iohk.iodb.LSMStore
import scorex.core.NodeViewComponentCompanion
import scorex.core.block.StateChanges
import scorex.core.settings.Settings
import scorex.core.transaction.TransactionChanges
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState

import scala.util.Try


case class SimpleBoxStoredState(store: LSMStore,
                           override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    HybridPersistentNodeViewModifier,
    SimpleBoxStoredState] {

  override type NVCT = SimpleBoxStoredState

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = ???

  private def dbVersion(ver: VersionTag) = Longs.fromByteArray(ver.take(8))

  private lazy val currentDbVersion = dbVersion(version)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] = ???

  override def boxOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  override def changes(transaction: SimpleBoxTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = ???

  override def changes(mod: HybridPersistentNodeViewModifier): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = ???

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox], newVersion: VersionTag): Try[SimpleBoxStoredState] = ???

  override def rollbackTo(version: VersionTag): Try[SimpleBoxStoredState] = Try {
    store.rollback(dbVersion(version))
    new SimpleBoxStoredState(store, version)
  }

  override def companion: NodeViewComponentCompanion = ???
}

object SimpleBoxStoredState{
  def emptyState(settings: Settings): SimpleBoxStoredState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    SimpleBoxStoredState(stateStorage, Array.emptyByteArray)
  }
}