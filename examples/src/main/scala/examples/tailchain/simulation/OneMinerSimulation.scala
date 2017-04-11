package examples.tailchain.simulation

import java.io.File

import com.google.common.primitives.Longs
import examples.commons.SimpleBoxTransaction
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.tailchain.modifiers.{BlockHeader, TBlock}
import examples.tailchain.utxo.AuthenticatedUtxo
import io.iohk.iodb.LSMStore
import scorex.core.transaction.state.PrivateKey25519Companion

import scala.util.Random


object OneMinerSimulation extends App {

  type Height = Int

  val blocksNum = 1000000
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

  val minerKeys = PrivateKey25519Companion.generateKeys(scorex.utils.randomBytes(32))
  val minerPubKey = minerKeys._2
  val minerPrivKey = minerKeys._1

  //creating genesis state & block
  val genesisBox = PublicKey25519NoncedBox(
    minerPubKey,
    Longs.fromByteArray(minerPubKey.pubKeyBytes.take(8)),
    100000000
  )

  val currentUtxoStore = new LSMStore(cuDir)
  val currentUtxo = AuthenticatedUtxo(currentUtxoStore, 1, None, defaultId)





  (1 to blocksNum) foreach {bn =>
    println("current height: " + currentHeight)
  }
}
