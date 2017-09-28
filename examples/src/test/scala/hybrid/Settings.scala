package hybrid

import examples.hybrid.mining.MiningSettings
import io.circe

import scala.concurrent.duration._

trait Settings {

  lazy val settings = new Settings with MiningSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile("settings.json")

    override lazy val targetBlockDelay: Long = 3.seconds.toMillis

    override lazy val Difficulty: BigInt = 1
  }

}
