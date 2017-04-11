package examples.tailchain.simulation

import java.io.File

import com.google.common.primitives.{Ints, Longs}
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionCompanion}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.tailchain.modifiers.{BlockHeader, TBlock}
import examples.tailchain.utxo.AuthenticatedUtxo
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.util.Random


object OneMinerSimulation extends App {
  import examples.tailchain.core.Constants.hashfn

  type Height = Int

  val blocksNum = 1
  val experimentId = Random.nextInt(5000)

  val headersChain = Map[Height, BlockHeader]()


  val bcDir = new File("/tmp/bc" + experimentId)
  bcDir.mkdirs()

  val fullBlocksStore = new LSMStore(bcDir)

  val defaultId = Array.fill(32)(0: Byte)

  def lastHeaderOpt: Option[(Int, BlockHeader)] = headersChain.lastOption

  def currentHeight: Int = lastHeaderOpt.map(_._1).getOrElse(0)

  def generateTransactions(utxo: AuthenticatedUtxo): Seq[SimpleBoxTransaction] = ???

  def generateBlock(): TBlock = ???

  val cuDir = new File("/tmp/cu" + experimentId)
  cuDir.mkdirs()

  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.Random.randomBytes(32))
  val minerPubKey = minerKeys._2
  val minerPrivKey = minerKeys._1

  //creating genesis state & block

  val genesisBoxes = (1 to 1000) map {i =>
    PublicKey25519NoncedBox(
      minerPubKey,
      Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
      10000000000L
    )
  }

  val currentUtxoStore = new LSMStore(cuDir)
  currentUtxoStore.update(
    ByteArrayWrapper(defaultId),
    Seq[ByteArrayWrapper](),
    genesisBoxes.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))
  )
  val currentUtxo = AuthenticatedUtxo(currentUtxoStore, genesisBoxes.size, None, defaultId)

  val txs = genesisBoxes.map { b =>
    SimpleBoxTransaction.apply(
      from = IndexedSeq(minerPrivKey -> b.nonce),
      to = IndexedSeq(minerPubKey -> 5, minerPubKey -> (b.value - 5)),
      0, System.currentTimeMillis())
  }

  val changes = AuthenticatedUtxo.changes(txs)
  currentUtxo.applyChanges()

  currentUtxo.a

  (1 to blocksNum) foreach {bn =>
    println("current height: " + currentHeight)
  }
}
