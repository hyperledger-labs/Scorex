package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Longs
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.PowBlock
import examples.hybrid.history.HybridHistory
import examples.hybrid.state.{HBoxStoredState, HBoxStoredState$}
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.utils.ScorexLogging


class PosForger(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  var forging = false

  override def receive: Receive = {
    case StartForging =>
      forging = true
      viewHolderRef ! GetCurrentView

    case CurrentView(h: HybridHistory, s: HBoxStoredState, w: HWallet, _) =>

      def hit(box:PublicKey25519NoncedBox) = {
        val h = FastCryptographicHash(box.bytes)  //pwb.bytes ++
        Longs.fromByteArray(h.take(8))
      }

      val boxes = w.boxes()
      println("boxes: " + boxes.size)

      val bw = h.bestPowBlock

      boxes.map(_.box).map(hit).foreach{hit =>
        println("hit: " + hit)
      }


      System.exit(150)

    case StopForging =>
      forging = false
  }
}

object PosForger {

  case object StartForging

  case object StopForging

}
