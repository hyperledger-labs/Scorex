package examples.curvepos.forging

import akka.actor.Actor

import scala.concurrent.duration._

class Forger extends Actor {
  import Forger._

  //set to true for initial generator
  private var forging = false

  val blockGenerationDelay = 500

  override def receive: Receive = {
    case StartMining =>
      forging = true
      context.system.scheduler.scheduleOnce(500.microsecond)(self ! Forge)
    case StopMining =>
      forging = false

    case Forge =>
      //todo: block generation
      context.system.scheduler.scheduleOnce(500.microsecond)(self ! Forge)
  }
}

object Forger {
  case object StartMining
  case object StopMining

  case object Forge
}