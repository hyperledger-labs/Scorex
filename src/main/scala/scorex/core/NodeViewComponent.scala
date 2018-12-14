package scorex.core

import akka.actor.{ActorContext, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import scorex.core.NodeViewComponentOperation.GetReader
import scorex.core.consensus.{HistoryReader, SyncInfo}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.transaction.state.StateReader
import scorex.core.transaction.wallet.VaultReader
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.util.ScorexLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}

object NodeViewComponent {
  trait ComponentType
  object StateComponent extends ComponentType
  object HistoryComponent extends ComponentType
  object MempoolComponent extends ComponentType
  object VaultComponent extends ComponentType
}
