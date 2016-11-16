package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.util.FileFunctions
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.utils.ScorexLogging


class PosForger(settings: Settings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
  val dataDir = dataDirOpt.get

  var forging = false

  val TransactionsPerBlock = 50

  def pickTransactions(memPool: HMemPool, state: HBoxStoredState): Seq[SimpleBoxTransaction] =
    memPool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransaction]()) { case (collected, tx) =>
      if (tx.valid &&
        tx.boxIdsToOpen.map(state.closedBox).forall(_.isDefined) &&
        tx.boxIdsToOpen.forall(id => !collected.flatMap(_.boxIdsToOpen).exists(_ sameElements id))) collected :+ tx
      else collected
    }


  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! GetCurrentView

    case CurrentView(h: HybridHistory, s: HBoxStoredState, w: HWallet, m: HMemPool) =>

      val target = MaxTarget / h.posDifficulty

      val boxes = w.boxes().map(_.box)
      println(s"${boxes.size} stakeholding outputs")

      //last check on whether to forge at all
      if (h.pairCompleted) {
        self ! StopForging
      } else {
        val powBlock = h.bestPowBlock
        posIteration(powBlock, boxes, pickTransactions(m, s), target) match {
          case Some(posBlock) =>
            println(s"locally generated PoS block: $posBlock")
            forging = false
            viewHolderRef !
              LocallyGeneratedModifier[PublicKey25519Proposition,
                SimpleBoxTransaction, HybridPersistentNodeViewModifier](posBlock)
          case None =>
            (1 to 10).foreach(_ => println("stuck"))
        }
      }

    case StopForging =>
      forging = false
  }
}

object PosForger extends ScorexLogging {
  val InitialDifficuly = 588281250L
  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

  def hit(pwb: PowBlock)(box: PublicKey25519NoncedBox): Long = {
    //    val h = FastCryptographicHash(pwb.bytes ++ box.bytes)
    val h: Array[Byte] = ???
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def posIteration(powBlock: PowBlock,
                   boxes: Seq[PublicKey25519NoncedBox],
                   txsToInclude: Seq[SimpleBoxTransaction],
                   target: Long
                  ): Option[PosBlock] = {
    val sucessfulHits = boxes.map { box =>
      val h = hit(powBlock)(box)
      (box.proposition, box.value, h)
    }.filter(t => t._3 < t._2 * target)

    log.info(s"Successful hits: ${sucessfulHits.size}")

    val record = s"${boxes.size}, $target, $sucessfulHits"
    FileFunctions.append("/home/kushti/posdata.csv", record)

    sucessfulHits.headOption.map { case (gen, _, _) =>
      PosBlock(
        powBlock.id,
        System.currentTimeMillis(),
        txsToInclude,
        gen,
        Signature25519(Array.fill(Signature25519.SignatureSize)(0: Byte)))
    }
  }
}
