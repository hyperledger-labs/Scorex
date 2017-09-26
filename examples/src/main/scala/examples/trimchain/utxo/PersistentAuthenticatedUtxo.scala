package examples.trimchain.utxo

import java.io.File

import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.trimchain.modifiers.{BlockHeader, TBlock, TModifier, UtxoSnapshot}
import examples.trimchain.utxo.PersistentAuthenticatedUtxo.ProverType
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.VersionTag
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChangeOperation, BoxStateChanges, Insertion, Removal}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup, Remove}
import scorex.crypto.authds.{ADKey, ADValue, SerializedAdProof}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.mid.state.BoxMinimalState

import scala.util.{Random, Success, Try}

trait AuthenticatedUtxo {

  import PublicKey25519NoncedBox.BoxKeyLength

  def prover: ProverType

  def lookupProof(ids: Seq[ADKey]): Try[SerializedAdProof] = Try {
    prover.generateProof() // todo: check prover's state in more elegant way, by calling something like ".isClean()"
    ids.foreach { id =>
      require(id.length == BoxKeyLength)
      val l = Lookup(id)
      prover.performOneOperation(l).get
    }
    prover.generateProof()
  }
}

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
case class PersistentAuthenticatedUtxo(store: LSMStore,
                                       size: Int,
                                       proverOpt: Option[ProverType], //todo: externalize the type with the parameter
                                       override val version: VersionTag) extends
  BoxMinimalState[PublicKey25519Proposition,
    PublicKey25519NoncedBox,
    SimpleBoxTransaction,
    TModifier,
    PersistentAuthenticatedUtxo] with AuthenticatedUtxo with ScorexLogging {

  import PublicKey25519NoncedBox.{BoxKeyLength, BoxLength}

  require(size >= 0)
  require(store.lastVersionID.map(_.data).getOrElse(version) sameElements version,
    s"${Base58.encode(store.lastVersionID.map(_.data).getOrElse(version))} != ${Base58.encode(version)}")

  override lazy val prover = proverOpt.getOrElse {
    val p = new ProverType(keyLength = BoxKeyLength, valueLengthOpt = Some(BoxLength)) //todo: feed it with genesis state
    log.debug("Starting building a tree for UTXO set")
    store.getAll { case (k, v) =>
      p.performOneOperation(Insert(ADKey @@ k.data, ADValue @@ v.data))
    }
    p.generateProof()
    log.debug("Finished building a tree for UTXO set")
    p
  }

  lazy val rootHash: VersionTag = VersionTag @@ prover.digest

  override type NVCT = PersistentAuthenticatedUtxo

  override def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = PersistentAuthenticatedUtxo.semanticValidity(tx)

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    store.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(PublicKey25519NoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  //there's no easy way to know boxes associated with a proposition, without an additional index
  override def boxesOf(proposition: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def changes(mod: TModifier): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
    PersistentAuthenticatedUtxo.changes(mod)

  //Validate transactions in block and generator box
  override def validate(mod: TModifier): Try[Unit] = Try {
    //    require(mod.parentId.sameElements(version))  todo: fix & uncomment

    mod match {
      case u: UtxoSnapshot => if (!this.isEmpty) throw new Exception("Utxo Set already imported")
      case _ =>
    }
    mod match {
      case b: TBlock => b.transactions.foreach(tx => validate(tx).ensuring(_.isSuccess))
      case _ =>
    }
  }

  //todo: newVersion is not used
  override def applyChanges(changes: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[PersistentAuthenticatedUtxo] = Try {

    val (boxIdsToRemove, boxesToAdd) = changes.operations
      .foldLeft(Seq[Array[Byte]]() -> Seq[PublicKey25519NoncedBox]()) { case ((btr, bta), op) =>
        op match {
          case Insertion(b) =>
            prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes))
            (btr, bta :+ b)
          case Removal(bid) =>
            assert(store.get(ByteArrayWrapper(bid)).isDefined)
            prover.performOneOperation(Remove(bid))
            (btr :+ bid, bta)
        }
      }

    prover.generateProof()

    val newVersion = rootHash

    log.debug(s"Update HBoxStoredState from version $lastVersionString to version ${Base58.encode(newVersion)}")

    val toRemove = boxIdsToRemove.map(ByteArrayWrapper.apply)
    val toAdd = boxesToAdd.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    store.update(ByteArrayWrapper(newVersion), toRemove, toAdd)

    val newSt = PersistentAuthenticatedUtxo(store, size + toAdd.size - toRemove.size, Some(prover), newVersion)
    assert(boxIdsToRemove.forall(box => newSt.closedBox(box).isEmpty), s"Removed box is still in state")
    assert(newSt.version sameElements newVersion, s"New version don't match")
    newSt
  }

  override def maxRollbackDepth: Int = store.keepVersions

  override def rollbackTo(version: VersionTag): Try[PersistentAuthenticatedUtxo] = Try {
    if (store.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback HBoxStoredState to ${Base58.encode(version)} from version $lastVersionString")
      store.rollback(ByteArrayWrapper(version))
      PersistentAuthenticatedUtxo(store, store.getAll().size, None, version) //todo: more efficient rollback, rollback prover
    }
  }

  private def lastVersionString = store.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  def isEmpty: Boolean = store.getAll().isEmpty

  def randomElement: Try[PublicKey25519NoncedBox] = Try {
    val bytes = store.getAll().drop(Random.nextInt(size - 1)).next()._2.data
    PublicKey25519NoncedBoxSerializer.parseBytes(bytes)
  }.flatten
}

object PersistentAuthenticatedUtxo {

  type ProverType = BatchAVLProver[Digest32, Blake2b256Unsafe]

  def semanticValidity(tx: SimpleBoxTransaction): Try[Unit] = Try {
    require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })
  }

  //todo: fees
  def changes(txs: Seq[SimpleBoxTransaction]): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] =
  Try {
    type SC = Seq[BoxStateChangeOperation[PublicKey25519Proposition, PublicKey25519NoncedBox]]

    val initial = (Seq(): SC, 0L) //no reward additional to tx fees

    //todo: reward is not used
    val (ops, reward) =
    txs.foldLeft(initial) { case ((os, f), tx) =>
      (os ++
        (tx.boxIdsToOpen.map(id => Removal[PublicKey25519Proposition, PublicKey25519NoncedBox](ADKey @@ id)): SC)
        ++ tx.newBoxes.map(b => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](b)): SC,
        f + tx.fee)
    }

    BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](ops)
  }

  def changes(mod: TModifier): Try[BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {

    mod match {
      case h: BlockHeader =>
        Success(BoxStateChanges(Seq()))

      case ps: TBlock =>
        changes(ps.transactions)

      //todo: implement
      case u: UtxoSnapshot => ???
    }
  }

  def readOrGenerate(settings: Settings): PersistentAuthenticatedUtxo = {
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
    val version = VersionTag @@ stateStorage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray)

    //todo: more efficient size detection, prover init
    PersistentAuthenticatedUtxo(stateStorage, stateStorage.getAll().size, None, version)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[TModifier]): PersistentAuthenticatedUtxo = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, VersionTag @@ mod.id)).get
    }
  }
}