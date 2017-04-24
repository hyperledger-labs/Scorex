package examples.spv

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.block.Block._
import scorex.crypto.encode.Base58

import scala.util.Try

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


  def constructSPVProof(m: Int, k: Int, blockchain: Seq[Header], inDifficulty: BigInt): Try[SPVProof] = Try {
    val (prefix: Seq[Header], suffix: Seq[Header]) = blockchain.splitAt(blockchain.length - k)
    val firstSuffix = suffix.head

    //TODO make efficient
    val blockchainMap: Map[ByteArrayWrapper, Header] = blockchain.map(b => ByteArrayWrapper(b.id) -> b).toMap
    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    def constructProof(i: Int): (Int, Seq[Header]) = {
      def loop(acc: Seq[Header]): Seq[Header] = {
        val interHeader = acc.head
        if (interHeader.interlinks.length > i) {
          val header = headerById(interHeader.interlinks(i))
          loop(header +: acc)
        } else {
          acc.reverse.tail.reverse
        }
      }
      val interchain = loop(Seq(firstSuffix))
      if (interchain.length >= m) {
        (i - 1, interchain)
      } else {
        constructProof(i - 1)
      }
    }
    val (depth, interchain) = constructProof(firstSuffix.interlinks.length)
    val difficulty: BigInt = inDifficulty * Math.pow(2, depth).toInt
    interchain.foreach(p => require(p.realDifficulty >= difficulty, s"${p.realDifficulty} >= $difficulty | ${Base58.encode(p.id)}"))

    SPVProof(m, k, depth, difficulty, interchain, suffix)
  }

}
