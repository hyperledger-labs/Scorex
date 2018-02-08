package scorex.core.utils

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.reflect.ClassTag

/**
  * Helper that encapsulates ask patter for actors and returns Future[_]
  */
trait ActorHelper {

  def askActor[A: ClassTag](actorRef: ActorRef, question: Any)
                           (implicit timeout: Timeout): Future[A] = (actorRef ? question).mapTo[A]

}
