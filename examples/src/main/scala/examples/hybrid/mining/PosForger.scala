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

      def hit(pwb: PowBlock)(box: PublicKey25519NoncedBox): Long = {
//        val h = FastCryptographicHash(pwb.bytes ++ box.bytes)
        val h: Array[Byte] = ???
        Longs.fromByteArray((0: Byte) +: h.take(7))
      }

      val boxes = w.boxes()
      println(s"${boxes.size} stakeholding outputs")

      //last check on whether to forge at all
      if (h.pairCompleted) {
        self ! StopForging
      } else {
        val powBlock = h.bestPowBlock

        boxes.map(_.box).map { box =>
          val h = hit(powBlock)(box)
          println(s"hit: $h, target $target, target * balance: ${box.value * target}, generated: ${h < box.value * target}")
          (box.proposition, box.value, h)
        }.find(t => t._3 < t._2 * target).map { case (gen, _, _) =>
          val txsToInclude = pickTransactions(m, s)

          PosBlock(
            powBlock.id,
            System.currentTimeMillis(),
            txsToInclude,
            gen,
            Signature25519(Array.fill(Signature25519.SignatureSize)(0: Byte)))
        } match {
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

object PosForger {
  val InitialDifficuly = 100000000L
  val MaxTarget = Long.MaxValue

  case object StartForging

  case object StopForging

}
