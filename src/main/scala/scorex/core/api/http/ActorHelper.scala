package scorex.core.api.http

import akka.pattern.ask
import akka.actor.ActorRef
import akka.util.Timeout

import scala.concurrent.Future
import scala.reflect.ClassTag

trait ActorHelper {

  def askActor[A: ClassTag](actorRef: ActorRef, question: Any)
                           (implicit timeout: Timeout): Future[A] = (actorRef ? question).mapTo[A]

}
