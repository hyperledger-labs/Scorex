package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridPersistentNodeViewModifier, PosBlock, PowBlock}
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import examples.hybrid.wallet.HWallet
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.utils.ScorexLogging


class PosForger(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  var forging = false

  val TransactionsPerBlock = 50

  def pickTransactions(memPool: HMemPool, state: HBoxStoredState): Seq[SimpleBoxTransaction] =
    memPool.take(TransactionsPerBlock).filter { tx =>
      tx.valid && tx.boxIdsToOpen.map(state.closedBox).forall(_.isDefined)
    }.toSeq


  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! GetCurrentView

    case CurrentView(h: HybridHistory, s: HBoxStoredState, w: HWallet, m: HMemPool) =>

      def hit(pwb: PowBlock)(box: PublicKey25519NoncedBox) = {
        val h = FastCryptographicHash(pwb.bytes ++ box.bytes)
        Longs.fromByteArray(h.take(8))
      }

      val boxes = w.boxes()

      val powBlock = h.bestPowBlock

      val hitOpt = boxes.map(_.box).map { box =>
        box.proposition -> hit(powBlock)(box)
      }.find(_._2 > PosDifficulty).map { case (gen, _) =>
        val txsToInclude = pickTransactions(m, s)

        PosBlock(
          powBlock.id,
          System.currentTimeMillis(),
          txsToInclude,
          gen,
          Signature25519(Array.fill(Signature25519.SignatureSize)(0:Byte)))
      } match {
        case Some(posBlock) =>
          viewHolderRef !
            LocallyGeneratedModifier[PublicKey25519Proposition,
              SimpleBoxTransaction, HybridPersistentNodeViewModifier](posBlock)
        case None =>
          println("stuck")
          System.exit(150)
      }

    case StopForging =>
      forging = false
  }
}

object PosForger {
  val PosDifficulty = 317429201286551790L

  case object StartForging

  case object StopForging

}
