package scorex.core.api.http

import akka.actor.ActorRef
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import scorex.core.NodeViewHolder.CurrentView

import scala.concurrent.Future

trait ApiRouteWithFullView[HIS, MS, VL, MP] extends ApiRoute {

  import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView

  val nodeViewHolderRef: ActorRef

  def withNodeView(f: CurrentView[HIS, MS, VL, MP] => Route): Route = onSuccess(viewAsync())(f)

  //TODO Data received in current view is mutable and may be inconsistent.
  //Better get concrete data you need from NodeViewHolder
  protected def viewAsync(): Future[CurrentView[HIS, MS, VL, MP]] = {
    def f(v: CurrentView[HIS, MS, VL, MP]) = v
    (nodeViewHolderRef ? GetDataFromCurrentView(f))
      .mapTo[CurrentView[HIS, MS, VL, MP]]
  }

}
