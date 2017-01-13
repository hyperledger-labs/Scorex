package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging


class PosForger(settings: Settings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
  val dataDir = dataDirOpt.get

  var forging = false

  val TransactionsPerBlock = 50

  def pickTransactions(memPool: HMemPool, state: HBoxStoredState): Seq[SimpleBoxTransaction] =
    memPool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransaction]()) { case (collected, tx) =>
      if (tx.isValid &&
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
      val boxKeys = boxes.flatMap(b => w.secretByPublicImage(b.proposition).map(s => (b, s)))
      log.debug(s"${boxes.size} stakeholding outputs")

      //last check on whether to forge at all
      if (h.pairCompleted) {
        self ! StopForging
      } else {
        val powBlock = h.bestPowBlock
        posIteration(powBlock, boxKeys, pickTransactions(m, s), target) match {
          case Some(posBlock) =>
            log.debug(s"Locally generated PoS block: $posBlock")
            forging = false
            viewHolderRef !
              LocallyGeneratedModifier[PublicKey25519Proposition,
                SimpleBoxTransaction, HybridBlock](posBlock)
          case None =>
            log.debug(s"Failed to generate PoS block")
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
    val h = FastCryptographicHash(pwb.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def posIteration(powBlock: PowBlock,
                   boxKeys: Seq[(PublicKey25519NoncedBox, PrivateKey25519)],
                   txsToInclude: Seq[SimpleBoxTransaction],
                   target: Long
                  ): Option[PosBlock] = {
    val successfulHits = boxKeys.map { boxKey =>
      val h = hit(powBlock)(boxKey._1)
      (boxKey, h)
    }.filter(t => t._2 < t._1._1.value * target)

    log.info(s"Successful hits: ${successfulHits.size}")

    successfulHits.headOption.map { case (boxKey, _) =>
      PosBlock.create(
        powBlock.id,
        System.currentTimeMillis(),
        txsToInclude,
        boxKey._1,
        boxKey._2)
    }
  }
}