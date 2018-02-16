package examples.trimchain.simulation

import java.io.{File, FileOutputStream, FileWriter}
import java.nio.file.{Files, Paths}

import com.google.common.primitives.{Ints, Longs}
import examples.commons.{PublicKey25519NoncedBox, SimpleBoxTransaction}
import examples.commons.{Nonce, Value}
import examples.trimchain.core.{Algos, Constants, TicketSerializer}
import examples.trimchain.modifiers.{BlockHeader, TBlock}
import io.iohk.iodb.ByteArrayWrapper
import io.iohk.iodb.Store.VersionID
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{BoxStateChanges, Insertion}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{Random, Try}


object OneMinerSimulation extends App with Simulators {

  import examples.trimchain.core.Constants.hashfn

  type Height = Int

  val experimentId = Random.nextInt(500000)
  val NewBoxesPerBlock = 5000

  val headersChain = mutable.Map[Height, BlockHeader]()

  val bcDir = new File("/tmp/oms/bc" + experimentId)
  bcDir.mkdirs()

  val fullBlocksStore = new MemoryFullBlockStore
  //  val fullBlocksStore = new LSMStore(bcDir, keySize = 4)

  def currentHeight: Int = Try(headersChain.keySet.max).getOrElse(0)

  val cuDir = new File("/tmp/oms/cu" + experimentId)
  cuDir.mkdirs()

  val muDir = new File("/tmp/oms/mu" + experimentId)
  muDir.mkdirs()

  def generateTransactions(richBoxes: Seq[PublicKey25519NoncedBox]): Seq[SimpleBoxTransaction] = {
    richBoxes.filter(_.value > 1).take(NewBoxesPerBlock).map { b =>
      SimpleBoxTransaction.apply(
        from = IndexedSeq(minerPrivKey -> b.nonce),
        to = IndexedSeq(minerPubKey -> Value @@ (b.value / 2), minerPubKey -> Value @@ (b.value - (b.value / 2))),
        0, System.currentTimeMillis())
    }
  }

  //creating genesis state & block

  val genesisBoxes = (1 to 5000) map { i =>
    PublicKey25519NoncedBox(
      minerPubKey,
      Nonce @@ Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
      Value @@ 10000000000L
    )
  }

  val genesisChanges: BoxStateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox] =
    BoxStateChanges(genesisBoxes.map(box => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](box)))

  var currentUtxo = InMemoryAuthenticatedUtxo(genesisBoxes.size, None, defaultId).applyChanges(genesisChanges, defaultId).get

  var miningHeight = 0
  var miningUtxo = InMemoryAuthenticatedUtxo(genesisBoxes.size, None, defaultId).applyChanges(genesisChanges, defaultId).get
    .ensuring(_.rootHash sameElements currentUtxo.rootHash)

  var generatingBoxes: Seq[PublicKey25519NoncedBox] = genesisBoxes

  log("Current height,Mining height,Current utxo size,Mining utxo size,Work valid,Header size,Ticket size,Proof size,Block size")

  val blocksNum = 10000
  (1 to blocksNum) foreach { _ =>
    val t0 = System.currentTimeMillis()

    val maybeNewMiningHeight = Algos.chooseSnapshots(currentHeight, minerPubKey.pubKeyBytes).headOption

    maybeNewMiningHeight match {
      case None =>
      case Some(newMiningHeight) =>
        if (newMiningHeight > miningHeight) {
          val mb = fullBlocksStore.get(ByteArrayWrapper(Ints.toByteArray(newMiningHeight))).get
          miningUtxo = miningUtxo.applyModifier(mb).get
          miningHeight = newMiningHeight
        }
    }

    val txs = generateTransactions(generatingBoxes)
    val (block, newBoxes, uu) = generateBlock(txs, currentUtxo, IndexedSeq(miningUtxo))
    generatingBoxes = newBoxes
    currentUtxo = uu

    val height = currentHeight + 1
    headersChain += height -> block.header

    val blockBytes = block.bytes
    val headerBytes = block.header.bytes

    val wvalid = Algos.validatePow(block.header, IndexedSeq(miningUtxo.rootHash), Constants.Difficulty)
    require(wvalid)
    log(s"$currentHeight,$maybeNewMiningHeight,${currentUtxo.size},${miningUtxo.size},$wvalid,${headerBytes.length},${TicketSerializer.toBytes(block.header.ticket).length},${block.header.ticket.partialProofs.headOption map (_.length)},${blockBytes.length}")

    fullBlocksStore.update(
      ByteArrayWrapper(Ints.toByteArray(height)),
      Seq(),
      Seq(ByteArrayWrapper(Ints.toByteArray(height)) -> block)
    )

    println((System.currentTimeMillis() - t0) / 1000.0 + " seconds")
  }

  def log(str: String): Unit = {
    println(str)
    val resultsFile = new FileWriter(s"/tmp/oms/results-$experimentId.csv", true)
    try {
      resultsFile.write(str + "\n")
    } finally {
      resultsFile.close()
    }
  }


  class FullBlockStore(dir: File, keySize: Int) {
    type K = ByteArrayWrapper
    type V = ByteArrayWrapper

    def update(versionID: VersionID, toRemove: Iterable[K], toUpdate: Iterable[(K, V)]): Unit = {
      toUpdate.foreach { tu =>
        val fos = new FileOutputStream(filenameFromKey(tu._1))
        fos.write(tu._2.data)
        fos.close()
      }
    }

    def get(key: K): Option[V] = {
      val path = Paths.get(filenameFromKey(key))
      Some(ByteArrayWrapper(Files.readAllBytes(path)))
    }

    private def filenameFromKey(k: K): String = dir.getAbsolutePath + "/block-" + Ints.fromByteArray(k.data)
  }

  class MemoryFullBlockStore {
    type K = ByteArrayWrapper
    type V = TBlock

    val map: TrieMap[K, V] = TrieMap()

    def update(versionID: VersionID, toRemove: Iterable[K], toUpdate: Iterable[(K, V)]): Unit = {
      toUpdate.foreach { tu =>
        map.put(tu._1, tu._2)
      }
    }

    def get(key: K): Option[V] = {
      map.get(key)
    }
  }

}