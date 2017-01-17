package examples.hybrid.mining

import io.circe.syntax._
import scorex.core.settings.Settings

import scala.concurrent.duration._

trait MiningSettings extends Settings with MiningConstants {
  lazy val BlockDelay: Long = if (isTestnet) 10.minutes.toMillis
  else 1.minute.toMillis

  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)

  override def toString: String = (Map("BlockDelay" -> BlockDelay.asJson) ++
    settingsJSON.map(s => s._1 -> s._2)).asJson.spaces2
}
