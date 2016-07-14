package scorex.consensus

import io.circe.Json

trait ConsensusSettings {
  val settingsJSON: Map[String, Json]

  private val DefaultHistory = "blockchain"
  lazy val history = settingsJSON.get("history").flatMap(_.asString).getOrElse(DefaultHistory)

  private val DefaultMaxRollback = 100
  lazy val MaxRollback = settingsJSON.get("max-rollback").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMaxRollback)
}
