package examples.spv

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.block.Block._

import scala.util.Try

object Algos {

  def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    examples.spv.Constants.MaxTarget / blockTarget
  }

  def constructInterlinks(parent: Header): Seq[Array[Byte]] = {
    val genesisId = parent.interlinks.head
    def generateInnerchain(curDifficulty: BigInt, acc: Seq[Array[Byte]]): Seq[Array[Byte]] = {
      if (parent.realDifficulty >= curDifficulty) {
        generateInnerchain(curDifficulty * 2, acc :+ parent.id)
      } else {
        parent.interlinks.find(pId => blockIdDifficulty(pId) >= curDifficulty) match {
          case Some(id) if !(id sameElements genesisId) => generateInnerchain(curDifficulty * 2, acc :+ id)
          case _ => acc
        }
      }
    }
    genesisId +: generateInnerchain(Constants.InitialDifficulty * 2, Seq[Array[Byte]]())
  }


  def constructKLS16Proof(m: Int, k: Int, blockchain: Seq[Header]): Try[KLS16Proof] = Try {
    require(m > 0 && m < blockchain.length, s"$m > 0 && $m < ${blockchain.length}")
    require(k > 0 && k < blockchain.length, s"$k > 0 && $k < ${blockchain.length}")

    val (_, suffix: Seq[Header]) = blockchain.splitAt(blockchain.length - k)
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
      val innerchain = loop(Seq(firstSuffix))
      if (innerchain.length >= m) (i, innerchain) else constructProof(i - 1)
    }

    val (depth, innerchain) = constructProof(firstSuffix.interlinks.length)

    KLS16Proof(m, k, depth, innerchain, suffix)
  }
}
