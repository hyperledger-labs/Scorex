package examples.hybrid.state

import java.io.File

import com.google.common.primitives.Longs
import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.curvepos.{Nonce, Value}
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.VersionTag
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChangeOperation, BoxStateChanges, Insertion, Removal}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds._
import scorex.crypto.encode.Base58
import scorex.mid.state.BoxMinimalState

import scala.util.{Failure, Success, Try}


case class HBoxStoredState(store: LSMStore, override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    HybridBlock,
    HBoxStoredState] with ScorexLogging {

  require(store.lastVersionID.map(_.data).getOrElse(version) sameElements version,
    s"${Base58.encode(store.lastVersionID.map(_.data).getOrElse(version))} != ${Base58.encode(version)}")

  override type NVCT = HBoxStoredState
  type HPMOD = HybridBlock

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = HBoxStoredState.semanticValidity(tx)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    store.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(PublicKey25519NoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  //there's no easy way to know boxes associated with a proposition, without an additional index
  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def changes(mod: HPMOD): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    HBoxStoredState.changes(mod)

  //Validate transactions in block and generator box
  override def validate(mod: HPMOD): Try[Unit] = Try {
    mod match {
      case b: PowBlock =>
        //coinbase transaction is generated implicitly when block is applied to state, no validation needed
        require((b.parentId sameElements version) || (b.prevPosId sameElements version)
          || b.brothers.exists(_.id sameElements version), s"Incorrect state version: ${Base58.encode(version)} " +
          s"found, (${Base58.encode(b.prevPosId)} || ${Base58.encode(b.parentId)} ||" +
          s" ${b.brothers.map(b => Base58.encode(b.id))}) expected")

      case b: PosBlock =>
        require(b.parentId sameElements version, s"Incorrect state version: ${Base58.encode(b.parentId)} " +
          s"found, ${Base58.encode(version)} expected.")
        closedBox(b.generatorBox.id).get
        b.transactions.foreach(tx => validate(tx).get)
    }
  }

  override def applyChanges(changes: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[HBoxStoredState] = Try {
    val boxIdsToRemove = changes.toRemove.map(_.boxId).map(ByteArrayWrapper.apply)
    val boxesToAdd = changes.toAppend.map(_.box).map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.trace(s"Update HBoxStoredState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")
    assert(store.lastVersionID.isEmpty || boxIdsToRemove.forall(i => closedBox(i.data).isDefined))
    store.update(ByteArrayWrapper(newVersion), boxIdsToRemove, boxesToAdd)
    val newSt = HBoxStoredState(store, newVersion)
    assert(boxIdsToRemove.forall(box => newSt.closedBox(box.data).isEmpty), s"Removed box is still in state")
    assert(newSt.version sameElements newVersion, s"New version don't match")
    newSt
  }

  override def maxRollbackDepth: Int = store.keepVersions

  override def rollbackTo(version: VersionTag): Try[HBoxStoredState] = Try {
    if (store.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback HBoxStoredState to ${Base58.encode(version)} from version $lastVersionString")
      store.rollback(ByteArrayWrapper(version))
      new HBoxStoredState(store, version)
    }
  }.recoverWith{case e =>
    log.error("Cant' do rollback: ", e)
    Failure[HBoxStoredState](e): Try[HBoxStoredState]
  }

  private def lastVersionString = store.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

}

object HBoxStoredState {
  def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = tx.semanticValidity

  def changes(mod: HybridBlock): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    mod match {
      case pb: PowBlock =>
        val proposition: PublicKey25519Proposition = pb.generatorProposition
        val nonce: Nonce = SimpleBoxTransaction.nonceFromDigest(mod.id)
        val value: Value = Value @@ 1L
        val minerBox = PublicKey25519NoncedBox(proposition, nonce, value)
        Success(BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Seq(Insertion(minerBox))))
      case ps: PosBlock =>
        Try {
          val initial = (Seq(): Seq[Array[Byte]], Seq(): Seq[PublicKey25519NoncedBox], 0L)

          val (toRemove: Seq[ADKey], toAdd: Seq[PublicKey25519NoncedBox], reward) =
            ps.transactions.foldLeft(initial) { case ((sr, sa, f), tx) =>
              ((sr ++ tx.boxIdsToOpen.toSet).map(id => ADKey @@ id), sa ++ tx.newBoxes.toSet, f + tx.fee)
            }

          //for PoS forger reward box, we use block Id as a nonce
          val forgerNonce = Nonce @@ Longs.fromByteArray(ps.id.take(8))
          val forgerBox = PublicKey25519NoncedBox(ps.generatorBox.proposition, forgerNonce, Value @@ reward)

          val ops: Seq[BoxStateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
            toRemove.map(id => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](id)) ++
              toAdd.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b)) ++
              Seq(Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](forgerBox))
          BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
        }
    }
  }

  def readOrGenerate(settings: Settings): HBoxStoredState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile, maxJournalEntryCount = 10000)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stateStorage.close()
      }
    })
    val version = VersionTag @@ stateStorage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray)

    HBoxStoredState(stateStorage, version)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[HybridBlock]): HBoxStoredState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, VersionTag @@ mod.id)).get
    }
  }
}