package examples.tailchain.utxo

import java.io.File

import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.tailchain.modifiers.{BlockHeader, TBlock, TModifier, UtxoSnapshot}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{Insertion, Removal, StateChangeOperation, StateChanges}
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup, Remove}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256Unsafe

import scala.util.{Random, Success, Try}
import AuthenticatedUtxo.ProverType

/**
  * We implement Ethereum-style authenticated UTXO for the moment, so all the parties replicate the full UTXO set
  * Thus we have only prover implemented
  * Further it would be a good idea to implement 2-party model from https://eprint.iacr.org/2016/994, so miners store
  * full state
  *
  * @param store
  * @param proverOpt
  * @param version
  */
case class AuthenticatedUtxo(store: LSMStore,
                             size: Int,
                             proverOpt: Option[ProverType], //todo: externalize the type with the parameter
                             override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    TModifier,
    AuthenticatedUtxo] with ScorexLogging {

  import PublicKey25519NoncedBox.{BoxKeyLength, BoxLength}

  assert(size >= 0)
  assert(store.lastVersionID.map(_.data).getOrElse(version) sameElements version,
    s"${Base58.encode(store.lastVersionID.map(_.data).getOrElse(version))} != ${Base58.encode(version)}")

  lazy val prover = proverOpt.getOrElse {
    val p = new ProverType(keyLength = BoxKeyLength, valueLength = BoxLength) //todo: feed it with genesis state
    log.debug("Starting building a tree for UTXO set")
    store.getAll { case (k, v) =>
      p.performOneOperation(Insert(k.data, v.data))
    }
    p.generateProof()
    log.debug("Finished building a tree for UTXO set")
    p
  }

  lazy val rootHash = prover.digest

  override type NVCT = AuthenticatedUtxo

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = AuthenticatedUtxo.semanticValidity(tx)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    store.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(PublicKey25519NoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  //there's no easy way to know boxes associated with a proposition, without an additional index
  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def changes(mod: TModifier): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    AuthenticatedUtxo.changes(mod)

  //Validate transactions in block and generator box
  override def validate(mod: TModifier): Try[Unit] = Try {
    assert(mod.parentId.sameElements(version))

    mod match {
      case u: UtxoSnapshot => if (!this.isEmpty) throw new Exception("Utxo Set already imported")
      case _ =>
    }
    super.validate(mod).get
  }

  override def applyChanges(changes: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[AuthenticatedUtxo] = Try {

    log.debug(s"Update HBoxStoredState from version $lastVersionString to version ${Base58.encode(newVersion)}")

    val (boxIdsToRemove, boxesToAdd) = changes.operations
      .foldLeft(Seq[Array[Byte]]() -> Seq[PublicKey25519NoncedBox]()) {case ((btr, bta), op) =>
      op match {
        case Insertion(b) =>
          prover.performOneOperation(Insert(b.id, b.bytes))
          (btr, bta :+ b)
        case Removal(bid) =>
          assert(store.get(ByteArrayWrapper(bid)).isDefined)
          prover.performOneOperation(Remove(bid))
          (btr :+ bid, bta)
      }
    }

    prover.generateProof()
    val toRemove = boxIdsToRemove.map(ByteArrayWrapper.apply)
    val toAdd = boxesToAdd.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))
    store.update(ByteArrayWrapper(newVersion), toRemove, toAdd)

    val newSt = AuthenticatedUtxo(store, size + toAdd.size - toRemove.size, Some(prover), newVersion)
    assert(boxIdsToRemove.forall(box => newSt.closedBox(box).isEmpty), s"Removed box is still in state")
    assert(newSt.version sameElements newVersion, s"New version don't match")
    newSt
  }

  override def rollbackTo(version: VersionTag): Try[AuthenticatedUtxo] = Try {
    if (store.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback HBoxStoredState to ${Base58.encode(version)} from version $lastVersionString")
      store.rollback(ByteArrayWrapper(version))
      AuthenticatedUtxo(store, store.getAll().size, None, version) //todo: more efficient rollback, rollback prover
    }
  }

  private def lastVersionString = store.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  def lookupProof(id: Array[Byte]): Try[Array[Byte]] = Try {
    require(id.length == BoxKeyLength)
    val l = Lookup(id)
    prover.generateProof() // todo: check prover's state in more elegant way, by calling something like ".isClean()"
    prover.performOneOperation(l).get
    prover.generateProof()
  }

  def isEmpty: Boolean = store.getAll().isEmpty

  def randomElement: Try[PublicKey25519NoncedBox] = Try{
    val bytes = store.getAll().drop(Random.nextInt(size-1)).next()._2.data
    PublicKey25519NoncedBoxSerializer.parseBytes(bytes)
  }.flatten
}

object AuthenticatedUtxo {

  type ProverType = BatchAVLProver[Blake2b256Unsafe]

  def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })
  }


  def changes(mod: TModifier): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    type SC = Seq[StateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]]

    mod match {
      case h: BlockHeader =>
        Success(StateChanges(Seq()))

      //todo: fees
      case ps: TBlock =>
        Try {
          val initial = (Seq(): SC, 0L)

          //todo: reward is not used
          val (ops, reward) =
            ps.transactions.map(_.foldLeft(initial) { case ((os, f), tx) =>
              (os ++
                (tx.boxIdsToOpen.map(id => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](id)): SC)
                ++ tx.newBoxes.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b)): SC,
                f + tx.fee)
            }).getOrElse((Seq(): SC, 0L)) //no reward additional to tx fees

          StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
        }

        //todo: implement
      case u: UtxoSnapshot => ???
    }
  }

  def readOrGenerate(settings: Settings): AuthenticatedUtxo = {
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
    val version = stateStorage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray)

    //todo: more efficient size detection, prover init
    AuthenticatedUtxo(stateStorage, stateStorage.getAll().size, None, version)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[TModifier]): AuthenticatedUtxo = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}