package scorex.core.network

sealed trait PenaltyType

object PenaltyType {
  case object MisbehaviorPenalty extends PenaltyType
  case object SpamPenalty extends PenaltyType
  case object PermanentPenalty extends PenaltyType
}
