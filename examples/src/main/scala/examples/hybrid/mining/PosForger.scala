package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetDataFromCurrentView}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random


class PosForger(settings: Settings with MiningSettings, viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
  val dataDir = dataDirOpt.get

  var forging = false

  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! getRequiredData

    case pfi: PosForgingInfo =>
      val target = MaxTarget / pfi.diff

      val boxKeys = pfi.boxKeys

      //last check on whether to forge at all
      if (pfi.pairCompleted) {
        self ! StopForging
      } else {
        val powBlock = pfi.bestPowBlock
        log.debug(s"Trying to generate PoS block on top of ${powBlock.encodedId} with balance " +
          s"${boxKeys.map(_._1.value).sum}")
        val attachment = Random.randomBytes(settings.posAttachmentSize)
        posIteration(powBlock, boxKeys, pfi.txsToInclude, attachment, target) match {
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
  val InitialDifficuly = 15000000000L
  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

  def hit(pwb: PowBlock)(box: PublicKey25519NoncedBox): Long = {
    val h = Blake2b256(pwb.bytes ++ box.bytes)
    Longs.fromByteArray((0: Byte) +: h.take(7))
  }

  def posIteration(powBlock: PowBlock,
                   boxKeys: Seq[(PublicKey25519NoncedBox, PrivateKey25519)],
                   txsToInclude: Seq[SimpleBoxTransaction],
                   attachment: Array[Byte],
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
        attachment,
        boxKey._2)
    }
  }

  val getRequiredData: GetDataFromCurrentView[HybridHistory,
    HBoxStoredState,
    HWallet,
    SimpleBoxTransactionMemPool,
    PosForgingInfo] = {
    val f: CurrentView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] => PosForgingInfo = {
      view: CurrentView[HybridHistory, HBoxStoredState, HWallet, SimpleBoxTransactionMemPool] =>

        val diff = view.history.posDifficulty
        val pairCompleted = view.history.pairCompleted
        val bestPowBlock = view.history.bestPowBlock
        val boxes = view.vault.boxes().map(_.box).filter(box => view.state.closedBox(box.id).isDefined)
        val boxKeys = boxes.flatMap(b => view.vault.secretByPublicImage(b.proposition).map(s => (b, s)))

        val txs = view.pool.take(TransactionsPerBlock).foldLeft(Seq[SimpleBoxTransaction]()) { case (collected, tx) =>
          if (view.state.validate(tx).isSuccess &&
            tx.boxIdsToOpen.forall(id => !collected.flatMap(_.boxIdsToOpen)
              .exists(_ sameElements id))) collected :+ tx
          else collected
        }

        PosForgingInfo(pairCompleted, bestPowBlock, diff, boxKeys, txs)
    }
    GetDataFromCurrentView[HybridHistory,
      HBoxStoredState,
      HWallet,
      SimpleBoxTransactionMemPool,
      PosForgingInfo](f)

  }

  val TransactionsPerBlock = 50

}

case class PosForgingInfo(pairCompleted: Boolean,
                          bestPowBlock: PowBlock,
                          diff: Long,
                          boxKeys: Seq[(PublicKey25519NoncedBox, PrivateKey25519)],
                          txsToInclude: Seq[SimpleBoxTransaction])
