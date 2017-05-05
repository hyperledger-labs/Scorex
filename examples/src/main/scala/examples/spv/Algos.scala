package examples.spv

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.block.Block._

import scala.annotation.tailrec
import scala.util.{Failure, Try}

object Algos {

  def blockIdDifficulty(id: Array[Version]): BigInt = {
    val blockTarget = BigInt(1, id)
    examples.spv.Constants.MaxTarget / blockTarget
  }

  def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
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

  //Algorithm 8 from the MKZ paper
  def constructMKZProof(m: Int, blockchain: Seq[Header]): Try[MKZProof] = Try {
    require(m > 0 && m < blockchain.length, s"$m > 0 && $m < ${blockchain.length}")

    val k = m //according to the desc
    val (prefix, suffix: Seq[Header]) = blockchain.splitAt(blockchain.length - k)

    //TODO make efficient
    val blockchainMap: Map[ByteArrayWrapper, Header] = blockchain.map(b => ByteArrayWrapper(b.id) -> b).toMap

    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    val i = prefix.last.interlinks.size - 1

    //Algorithm 6 from the MKZ paper
    def constructInnerChain(c: Seq[Header], i: Int, boundary: Int): Seq[Header] = {

      @tailrec
      def stepThroughInnerchain(b: Header, level: Int, collected: Seq[Header], boundary: Int): Seq[Header] = {
        if (b.interlinks.size <= level || collected.size == boundary) collected else {
          val blockId = b.interlinks(i)
          val newB = headerById(blockId)
          stepThroughInnerchain(newB, level, collected :+ newB, boundary)
        }
      }

      stepThroughInnerchain(c.last, i, Seq(), boundary)
    }


    val topSuperchain = constructInnerChain(prefix, i, Int.MaxValue)
    println("topchain: " + topSuperchain.length)

    val chainsDown = (i - 1).to(0, -1).map { ci =>
      constructInnerChain(prefix, ci, m)
    }

    val proofChains = Seq(topSuperchain) ++ chainsDown

    val proofBytes = proofChains.reduce(_ ++ _).map(_.bytes).reduce(_ ++ _)

    println("proof bytes:" + proofBytes.length)

    MKZProof(m, k, proofChains, suffix)
  }

  /**
    * Constructs SPV Proof from KLS16 paper
    *
    * @param m          - parameter "m" from the paper (minimal length of innerchain to include)
    * @param k          - parameter "k" from the paper (chain suffix)
    * @param blockchain - chain of headers to construct a proof from
    * @return
    */
  def constructKLS16Proof(m: Int, k: Int, blockchain: Seq[Header]): Try[KLS16Proof] = Try {
    require(m > 0 && m < blockchain.length, s"$m > 0 && $m < ${blockchain.length}")
    require(k > 0 && k < blockchain.length, s"$k > 0 && $k < ${blockchain.length}")

    val (_, suffix: Seq[Header]) = blockchain.splitAt(blockchain.length - k)
    val firstSuffix = suffix.head

    //TODO make efficient
    val blockchainMap: Map[ByteArrayWrapper, Header] = blockchain.map(b => ByteArrayWrapper(b.id) -> b).toMap

    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    @tailrec
    def constructProof(i: Int): (Int, Seq[Header]) = {
      @tailrec
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
