package examples.spv.simulation

import com.google.common.primitives.{Ints, Longs}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.spv.{Algos, Header}
import examples.trimchain.core.Constants._
import examples.trimchain.simulation.InMemoryAuthenticatedUtxo
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{Insertion, StateChanges}

import scala.annotation.tailrec
import scala.util.Random

trait SimulatorFuctions {
  val defaultId = Array.fill(32)(0: Byte)

  def genGenesisState(minerPubKey: PublicKey25519Proposition): InMemoryAuthenticatedUtxo = {
    val genesisBoxes = (1 to 5000) map { i =>
      PublicKey25519NoncedBox(
        minerPubKey,
        Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
        10000000000L
      )
    }
    val genesisChanges: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox] =
      StateChanges(genesisBoxes.map(box => Insertion[PublicKey25519Proposition, PublicKey25519NoncedBox](box)))
    InMemoryAuthenticatedUtxo(genesisBoxes.size, None, defaultId).applyChanges(genesisChanges, defaultId).get
  }

  @tailrec
  final def genChain(height: Int, difficulty: BigInt, stateRoot: Array[Byte], genesis: Header, acc: IndexedSeq[Header]): Seq[Header] = if (height == 0) {
    acc.reverse
  } else {
    val block = genBlock(difficulty, genesis, acc, stateRoot, defaultId, System.currentTimeMillis())
    genChain(height - 1, difficulty, stateRoot, genesis, block +: acc)
  }


  def genBlock(difficulty: BigInt,
               genesis: Header,
               parents: IndexedSeq[Header],
               stateRoot: Array[Version],
               transactionsRoot: Array[Version],
               timestamp: Timestamp): Header = {
    val parent = parents.head
    def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parent.realDifficulty >= curDifficulty) {
        generateInnerchain(curDifficulty * 2, acc :+ parent.id)
      } else {
        parent.innerchainLinks.find(pId => Algos.blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) if !(id sameElements genesis.id) => generateInnerchain(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }
    val innerChainLinksIds: Seq[Array[Byte]] = genesis.id +: generateInnerchain(difficulty, Seq[Array[Byte]]())


    @tailrec
    def generateHeader(): Header = {
      val nonce = Random.nextInt
      val header = Header(parent.id, innerChainLinksIds, stateRoot, transactionsRoot, timestamp, nonce)
      if (correctWorkDone(header.id, difficulty)) header
      else generateHeader()
    }
    generateHeader()
  }

  def correctWorkDone(id: Array[Version], difficulty: BigInt): Boolean = {
    val target = examples.spv.Constants.MaxTarget / difficulty
    BigInt(1, id) < target
  }

  def genGenesisHeader(stateRoot: Array[Byte], minerPubKey: PublicKey25519Proposition): Header = {
    val genesisBoxes = (1 to 5000) map { i =>
      PublicKey25519NoncedBox(
        minerPubKey,
        Longs.fromByteArray(hashfn(minerPubKey.pubKeyBytes ++ Ints.toByteArray(i)).take(8)),
        10000000000L
      )
    }

    Header(Array.fill(32)(0: Byte), Seq(), stateRoot, defaultId, 0L, 0)
  }

}
