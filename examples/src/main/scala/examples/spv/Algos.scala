package examples.spv

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.block.Block._

object Algos {

  def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    examples.spv.Constants.MaxTarget / blockTarget
  }

  def constructInterlinks(parent: Header, genesis: Header, initialDifficulty: BigInt): Seq[Array[Byte]] = {
    def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parent.realDifficulty >= curDifficulty) {
        generateInnerchain(curDifficulty * 2, acc :+ parent.id)
      } else {
        parent.interlinks.find(pId => Algos.blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) if !(id sameElements genesis.id) => generateInnerchain(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }
    genesis.id +: generateInnerchain(initialDifficulty, Seq[Array[Byte]]())
  }

  def constructSPVProof(m: Int, blockchain: Map[ByteArrayWrapper, Header]): SPVProof = {
    ???
  }

}
