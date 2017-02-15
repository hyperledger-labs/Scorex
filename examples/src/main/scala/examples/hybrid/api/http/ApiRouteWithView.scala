package examples.hybrid.api.http

import akka.actor.ActorRef
import akka.pattern.ask
import examples.hybrid.history.HybridHistory
import examples.hybrid.mempool.HMemPool
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.ApiRoute

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  protected def getView(): Future[CurrentView[HybridHistory, HBoxStoredState, HWallet, HMemPool]] = {
    (nodeViewHolderRef ? GetCurrentView)
      .mapTo[CurrentView[HybridHistory, HBoxStoredState, HWallet, HMemPool]]
  }

}
