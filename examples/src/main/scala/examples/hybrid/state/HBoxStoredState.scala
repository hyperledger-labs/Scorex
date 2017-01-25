package examples.hybrid.state

import java.io.File

import com.google.common.primitives.Longs
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Success, Try}


case class HBoxStoredState(store: LSMStore, override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    HybridBlock,
    HBoxStoredState] with ScorexLogging {

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

  override def changes(mod: HPMOD): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    HBoxStoredState.changes(mod)

  //Validate transactions in block and generator box
  //todo: move validation to history
  override def validate(mod: HPMOD): Try[Unit] = Try {
    super.validate(mod).get
    mod match {
      case b: PowBlock => //coinbase transaction is generated implicitly when block is applied to state
      case b: PosBlock =>
        closedBox(b.generatorBox.id).get
        mod.transactions.getOrElse(Seq()).foreach(tx => validate(tx).get)
    }
  }


  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[HBoxStoredState] = Try {
    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.debug(s"Update HBoxStoredState from version ${store.lastVersionID} to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")
    if (store.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))
    store.update(ByteArrayWrapper(newVersion), boxIdsToRemove, boxesToAdd)
    val newSt = HBoxStoredState(store, newVersion)
    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt
  }

  override def rollbackTo(version: VersionTag): Try[HBoxStoredState] = Try {
    if (store.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback HBoxStoredState to ${Base58.encode(version)} from version ${store.lastVersionID}")
      store.rollback(ByteArrayWrapper(version))
      new HBoxStoredState(store, version)
    }
  }
}

object HBoxStoredState {
  def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })
  }


  def changes(mod: HybridBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    mod match {
      case pb: PowBlock =>
        val proposition: PublicKey25519Proposition = pb.generatorProposition
        val nonce: Long = SimpleBoxTransaction.nonceFromDigest(mod.id)
        val value: Long = 1
        val toAdd = PublicKey25519NoncedBox(proposition, nonce, value)
        Success(StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set(toAdd)))
      case ps: PosBlock =>
        Try {
          val initial = (Set(): Set[Array[Byte]], Set(): Set[PublicKey25519NoncedBox], 0L)

          val (toRemove: Set[Array[Byte]], toAdd: Set[PublicKey25519NoncedBox], reward) =
            ps.transactions.map(_.foldLeft(initial) { case ((sr, sa, f), tx) =>
              (sr ++ tx.boxIdsToOpen.toSet, sa ++ tx.newBoxes.toSet, f + tx.fee)
            }).getOrElse((Set(), Set(), 0L)) //no reward additional to tx fees

          //for PoS forger reward box, we use block Id as a nonce
          val forgerNonce = Longs.fromByteArray(ps.id.take(8))
          val forgerBox = PublicKey25519NoncedBox(ps.generatorBox.proposition, forgerNonce, reward)
          StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAdd ++ Set(forgerBox))
        }
    }
  }

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

    HBoxStoredState(stateStorage, Array.emptyByteArray)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[HybridBlock]): HBoxStoredState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}