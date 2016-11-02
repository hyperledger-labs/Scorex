package examples.hybrid.mining

import akka.actor.{Actor, ActorRef}
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.block.Block._
import scorex.core.utils.ScorexLogging


class PosForger(viewHolderRef: ActorRef) extends Actor with ScorexLogging {

  import PosForger._

  var forging = false
  var parentId: Array[Byte] = Array.empty

  override def receive: Receive = {
    case StartForging(pId) =>
      forging = true
      parentId = pId
      viewHolderRef ! GetCurrentView

    case CurrentView(_, _, w: HWallet, _) =>
      w.boxes()

    case StopForging =>
      forging = false
  }
}

object PosForger {

  case class StartForging(parentId: BlockId)

  case object StopForging

}
