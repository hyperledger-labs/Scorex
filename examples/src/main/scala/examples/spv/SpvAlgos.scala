package examples.spv

import io.iohk.iodb.ByteArrayWrapper
import scorex.core.utils.ScorexEncoding

import scala.annotation.tailrec
import scala.util.Try

object SpvAlgos extends ScorexEncoding {

  def blockIdDifficulty(id: Array[Byte]): BigInt = {
    val blockTarget = BigInt(1, id)
    examples.spv.Constants.MaxTarget / blockTarget
  }

  def constructInterlinkVector(parent: Header): Seq[Array[Byte]] = {
    // TODO: fixme, What should we do if `parent.interlinks` is empty? Can we return an empty sequence here?
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val genesisId = parent.interlinks.head

    @tailrec
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

  def constructKMZProof(m: Int, k: Int, C: Seq[Header]): Try[KMZProof] = Try {
    require(m > 0 && m < C.length, s"$m > 0 && $m < ${C.length}")
    require(k > 0 && k < C.length, s"$k > 0 && $k < ${C.length}")

    val (prefix, suffix: Seq[Header]) = C.splitAt(C.length - k)

    // TODO: fixme, What would be a more meaningful name for `i`? We also need a default value when `prefix` is empty
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val i = prefix.last.interlinks.size - 1
    val blockchainMap: Map[ByteArrayWrapper, Header] = C.map(b => ByteArrayWrapper(b.id) -> b).toMap
    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    //Algorithm 3 from the KMZ paper
    @tailrec
    def prove(boundary: Header, i: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      if (i == 0) {
        acc
      } else {
        // TODO: `suffix.head` seems to be safe due to the requirement that k < C.length
        //       at the definition of `suffix`
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val firstSuffix: Header = suffix.head
        val inC: Seq[Header] = constructInnerChain(firstSuffix, i, boundary, headerById)
          .ensuring(_.forall(h => h.realDifficulty >= i * Constants.InitialDifficulty))
        val (newIn, newB) = if (inC.length >= m) {
          (constructInnerChain(firstSuffix, i, inC(inC.length - m), headerById), inC(inC.length - m))
        } else {
          (inC, boundary)
        }

        val newAcc: Seq[Seq[Header]] = if (newIn.length >= m) acc ++ Seq(newIn) else acc
        prove(newB, i - 1, newAcc)
      }
    }

    // TODO: `C.head` seems to be safe as it is required that
    //       `0 < m < C.longth` and `0 < k < C.longth` at
    //        the begining of this method
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val boundary = C.head
    val proofChains = prove(boundary, i, Seq())

    KMZProof(m, k, proofChains, suffix)
  }

  //Algorithm 3 from the KMZ paper
  def constructInnerChain(startBlock: Header,
                          mu: Int,
                          boundary: Header,
                          headerById: Array[Byte] => Header): Seq[Header] = {

    @tailrec
    def stepThroughInnerchain(B: Header, collected: Seq[Header], boundary: Header): Seq[Header] = {
      val blockIdTry = Try(B.interlinks(mu))

      if (B.encodedId == boundary.encodedId || blockIdTry.isFailure) {
        collected
      } else {
        val blockId = blockIdTry.get
        val newB = headerById(blockId)
        stepThroughInnerchain(newB, collected :+ newB, boundary)
      }
    }
    stepThroughInnerchain(startBlock, Seq(), boundary).reverse
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
    // TODO: `firstSuffix.head` seems to be safe due to th requirement that k < blockchain.length
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val firstSuffix = suffix.head

    //TODO make efficient
    val blockchainMap: Map[ByteArrayWrapper, Header] = blockchain.map(b => ByteArrayWrapper(b.id) -> b).toMap

    def headerById(id: Array[Byte]): Header = blockchainMap(ByteArrayWrapper(id))

    @tailrec
    def constructProof(i: Int): (Int, Seq[Header]) = {

      // TODO: `acc` is never empty here, we may add a require stating so
      @tailrec
      @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
      def loop(acc: Seq[Header]): Seq[Header] = {
        val interHeader = acc.head
        if (interHeader.interlinks.length > i) {
          val header = headerById(interHeader.interlinks(i))
          loop(header +: acc)
        } else {
          acc.init
        }
      }

      val innerchain = loop(Seq(firstSuffix))
      if (innerchain.length >= m) (i, innerchain) else constructProof(i - 1)
    }

    val (depth, innerchain) = constructProof(firstSuffix.interlinks.length)

    KLS16Proof(m, k, depth, innerchain, suffix)
  }

  //debug method
  def stringChain(c: Seq[Header]): String = c.map(h => h.realDifficulty + "-" + encoder.encode(h.id).take(4)).mkString(",")

}
