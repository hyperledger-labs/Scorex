package scorex.core.network.peer

/**
  * A trait describing all possible types of the network participant misbehavior.
  * `penaltyScore` - a number defining how bad concrete kind of misbehavior is,
  * `isPermanent`  - a flag defining whether a penalty is permanent.
  */
sealed trait PenaltyType {
  val penaltyScore: Int
  val isPermanent: Boolean = false
}

object PenaltyType {

  case object NonDeliveryPenalty extends PenaltyType {
    override val penaltyScore: Int = 2
  }

  case object MisbehaviorPenalty extends PenaltyType {
    override val penaltyScore: Int = 10
  }

  case object SpamPenalty extends PenaltyType {
    override val penaltyScore: Int = 25
  }

  case object PermanentPenalty extends PenaltyType {
    override val penaltyScore: Int = 1000000000
    override val isPermanent: Boolean = true
  }

}
