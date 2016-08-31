package scorex.core.consensus

import org.h2.mvstore.{MVMap, MVStore}
import scorex.core.block.{Block, BlockCompanion, ConsensusData}
import scorex.core.serialization.BytesParseable
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.Transaction
import scorex.core.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}


/**
 * If no datafolder provided, blockchain lives in RAM (useful for tests)
 */
trait StoredBlockchain[P <: Proposition, TX <: Transaction[P, TX], B <: Block[P, TX]]
  extends BlockChain[P, TX] with ScorexLogging {

  val dataFolderOpt: Option[String]

  val blockCompanion: BlockCompanion[P, TX, B]

  case class BlockchainPersistence(database: MVStore) {
    val blocks: MVMap[Int, Array[Byte]] = database.openMap("blocks")
    val signatures: MVMap[Int, BlockId] = database.openMap("signatures")
    val scoreMap: MVMap[Int, BigInt] = database.openMap("score")

    //if there are some uncommited changes from last run, discard'em
    if (signatures.size() > 0) database.rollback()

    def writeBlock(height: Int, block: B): Try[Unit] = Try {
      blocks.put(height, block.bytes)
      scoreMap.put(height, score() + block.companion.score(block, this))
      signatures.put(height, block.id)
      database.commit()
    }

    def readBlock(height: Int): Option[B] =
      Try(Option(blocks.get(height)))
        .toOption
        .flatten
        .flatMap { b =>
          blockCompanion.parse(b) match {
            case Failure(e) =>
              log.error("Failed to parse block.", e)
              None
            case Success(block) => Some(block)
          }
        }

    def deleteBlock(height: Int): Unit = {
      blocks.remove(height)
      signatures.remove(height)
      database.commit()
    }

    def contains(id: BlockId): Boolean = signatures.exists(_._2.sameElements(id))

    def height(): Int = signatures.size()

    def heightOf(id: BlockId): Option[Int] = signatures.find(_._2.sameElements(id)).map(_._1)

    def score(): BigInt = if (height() > 0) scoreMap.get(height()) else 0
  }

  private lazy val blockStorage: BlockchainPersistence = {
    val db = dataFolderOpt match {
      case Some(dataFolder) => new MVStore.Builder().fileName(dataFolder + s"/blocks.mvstore").compress().open()
      case None => new MVStore.Builder().open()
    }
    BlockchainPersistence(db)
  }

  override def appendBlock(block: B): Try[History[P, TX]] = synchronized {
    Try {
      val parent = block.parentId
      if ((height() == 0) || (lastBlock.id sameElements parent)) {
        val h = height() + 1
        blockStorage.writeBlock(h, block) match {
          case Success(_) => Seq(block)
          case Failure(t) => throw new Error("Error while storing blockchain a change: " + t)
        }
      } else {
        throw new Error(s"Appending block ${block.json} which parent is not last block in blockchain")
      }
      this
    }
  }

  override def discardBlock(): Try[History[P, TX]] = synchronized {
    Try {
      require(height() > 1, "Chain is empty or contains genesis block only, can't make rollback")
      val h = height()
      blockStorage.deleteBlock(h)
      this
    }
  }

  override def blockAt(height: Int): Option[Block[P, TX]] = synchronized {
    blockStorage.readBlock(height)
  }

  override def lastBlockIds(howMany: Int): Seq[BlockId] =
    (Math.max(1, height() - howMany + 1) to height()).flatMap(i => Option(blockStorage.signatures.get(i))).reverse

  override def contains(signature: Array[Byte]): Boolean = blockStorage.contains(signature)

  override def height(): Int = blockStorage.height()

  override def score(): BigInt = blockStorage.score()

  override def heightOf(blockSignature: BlockId): Option[Int] = blockStorage.heightOf(blockSignature)

  override def blockById(blockId: BlockId): Option[Block[P, TX]] = heightOf(blockId).flatMap(blockAt)

  override def children(blockId: BlockId): Seq[Block[P, TX]] =
    heightOf(blockId).flatMap(h => blockAt(h + 1)).toSeq
}