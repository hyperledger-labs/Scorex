package examples.spv.simulation

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.spv.Header
import examples.trimchain.core.Constants._
import examples.trimchain.simulation.InMemoryAuthenticatedUtxo
import examples.trimchain.simulation.OneMinerSimulation._
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{Insertion, StateChanges}
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.util.Random

object SPVSimulator extends App with ScorexLogging {

  val Difficulty = BigInt(1000)
  val defaultId = Array.fill(32)(0: Byte)
  val genesisBoxes = (1 to 5000) map { i =>
    PublicKey25519NoncedBox(
      minerPubKey,
      Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
      10000000000L
    )
  }
  val genesisChanges: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox] =
    StateChanges(genesisBoxes.map(box => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](box)))
  val genesisUtxo = InMemoryAuthenticatedUtxo(genesisBoxes.size, None, defaultId).applyChanges(genesisChanges, defaultId).get
  val stateRoot = genesisUtxo.rootHash

  val genesisHeader: Header = Header(defaultId: BlockId, Seq(), stateRoot, defaultId, 0L, 0)

  val headerChain = genChain(100, Seq(genesisHeader))
  headerChain.reverse.map(println)


  @tailrec
  def genChain(height: Int, acc: Seq[Header]): Seq[Header] = if (height == 0) {
    acc
  } else {
    val block = genBlock(Difficulty, acc.head.id, stateRoot, defaultId, System.currentTimeMillis())
    genChain(height - 1, block +: acc)
  }


  def genBlock(difficulty: BigInt,
               parentId: BlockId,
               stateRoot: Array[Byte],
               transactionsRoot: Array[Byte],
               timestamp: Block.Timestamp): Header = {
    val innerChainLinks: Seq[Array[Byte]] = ???
    @tailrec
    def loop(): Header = {
      val nonce = Random.nextInt
      val header = Header(parentId, innerChainLinks, stateRoot, transactionsRoot, timestamp, nonce)
      if (correctWorkDone(header.id, difficulty)) header
      else loop()
    }
    loop()
  }

  def correctWorkDone(id: Array[Byte], difficulty: BigInt): Boolean = {
    val target = examples.spv.Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }
}
