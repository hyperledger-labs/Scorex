package examples.tailchain.simulation

import java.io.File

import com.google.common.primitives.{Ints, Longs}
import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.tailchain.core.{Algos, Constants, TicketSerializer}
import examples.tailchain.modifiers.{BlockHeader, TBlock, TBlockSerializer}
import examples.tailchain.utxo.AuthenticatedUtxo
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.collection.mutable
import scala.util.{Random, Try}


object OneMinerSimulation extends App {

  import examples.tailchain.core.Constants.hashfn

  type Height = Int

  val experimentId = Random.nextInt(500000)

  val headersChain = mutable.Map[Height, BlockHeader]()

  val bcDir = new File("/tmp/bc" + experimentId)
  bcDir.mkdirs()

  val fullBlocksStore = new LSMStore(bcDir, keySize = 4)

  val defaultId = Array.fill(32)(0: Byte)

  def currentHeight: Int = Try(headersChain.keySet.max).getOrElse(0)

  def generateTransactions(richBoxes: Seq[PublicKey25519NoncedBox]): Seq[SimpleBoxTransaction] =
    richBoxes.map { b =>
      SimpleBoxTransaction.apply(
        from = IndexedSeq(minerPrivKey -> b.nonce),
        to = IndexedSeq(minerPubKey -> 5, minerPubKey -> (b.value - 5)),
        0, System.currentTimeMillis())
    }

  def generateBlock(txs: Seq[SimpleBoxTransaction],
                    currentUtxo: AuthenticatedUtxo,
                    miningUtxos: IndexedSeq[AuthenticatedUtxo]): (TBlock, Seq[PublicKey25519NoncedBox], AuthenticatedUtxo) = {
    //todo: fix, hashchain instead of Merkle tree atm
    val txsHash = hashfn(txs.map(_.bytes).reduce(_ ++ _))

    val changes = AuthenticatedUtxo.changes(txs).get
    val updUtxo = currentUtxo.applyChanges(changes, scorex.utils.Random.randomBytes()).get

    val h = Algos.pow(defaultId, txsHash, currentUtxo.rootHash, minerPubKey.pubKeyBytes,
      miningUtxos, Constants.Difficulty, 10000).get.get

    val newRichBoxes = txs.flatMap(_.newBoxes).filter(_.value > 5)
    (TBlock(h, txs, System.currentTimeMillis()), newRichBoxes, updUtxo)
  }

  val cuDir = new File("/tmp/cu" + experimentId)
  cuDir.mkdirs()

  val muDir = new File("/tmp/mu" + experimentId)
  muDir.mkdirs()


  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.Random.randomBytes(32))
  val minerPubKey = minerKeys._2
  val minerPrivKey = minerKeys._1

  //creating genesis state & block

  val genesisBoxes = (1 to 5000) map { i =>
    PublicKey25519NoncedBox(
      minerPubKey,
      Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
      10000000000L
    )
  }

  val currentUtxoStore = new LSMStore(cuDir, keepVersions = 1)
  currentUtxoStore.update(
    ByteArrayWrapper(defaultId),
    Seq[ByteArrayWrapper](),
    genesisBoxes.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))
  )
  var currentUtxo = AuthenticatedUtxo(currentUtxoStore, genesisBoxes.size, None, defaultId)


  val miningUtxoStore = new LSMStore(muDir, keepVersions = 1)
  miningUtxoStore.update(
    ByteArrayWrapper(defaultId),
    Seq[ByteArrayWrapper](),
    genesisBoxes.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))
  )

  var miningHeight = 0
  var miningUtxo = AuthenticatedUtxo(miningUtxoStore, genesisBoxes.size, None, defaultId)

  var generatingBoxes: Seq[PublicKey25519NoncedBox] = genesisBoxes

  val blocksNum = 500
  (1 to blocksNum) foreach { bn =>
    println("current height: " + currentHeight)

    val newMiningHeight = Algos.chooseSnapshots(currentHeight, minerPubKey.pubKeyBytes).head

    println("mining height: " + newMiningHeight)

    if(newMiningHeight > miningHeight){
      val mbb = fullBlocksStore.get(ByteArrayWrapper(Ints.toByteArray(newMiningHeight))).get.data
      val mb = TBlockSerializer.parseBytes(mbb).get
      miningUtxo = miningUtxo.applyModifier(mb).get
      miningHeight = newMiningHeight
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
    println("Work valid: " + wvalid)

    println(s"Current utxo size: ${currentUtxo.size}")
    println(s"Mining utxo size: ${miningUtxo.size}")
    println(s"Header size: ${headerBytes.length}")
    println(s"Ticket size: ${TicketSerializer.toBytes(block.header.ticket).length}")
    println(s"Proof size: ${block.header.ticket.partialProofs.head.length}")
    println(s"Block size: ${blockBytes.length}")

    fullBlocksStore.update(
      ByteArrayWrapper(Ints.toByteArray(height)),
      Seq(),
      Seq(ByteArrayWrapper(Ints.toByteArray(height)) -> ByteArrayWrapper(blockBytes))
    )
  }

  println(s"Headers chain: $headersChain")
}