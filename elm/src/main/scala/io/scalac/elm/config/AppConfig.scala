package io.scalac.elm.config

import com.typesafe.config.{Config, ConfigFactory, ConfigObject, ConfigRenderOptions}
import io.circe.{Json, parser}
import io.scalac.elm.config.AppConfig._
import scorex.core.settings.Settings

object AppConfig {
  case class Genesis(generate: Boolean, initialFunds: Long)

  def load(): AppConfig = {
    val root = ConfigFactory.load()
    val elm = root.getConfig("elm")

    AppConfig(
      settings = settings(root),
      genesis = genesis(elm.getConfig("genesis"))
    )
  }

  private def settings(config: Config) = new Settings {
    val settingsJSON = config2Json(config.getObject("scorex"))
  }

  private def genesis(config: Config) = Genesis(
    generate = config.getBoolean("generate"),
    initialFunds = config.getLong("initial-funds")
  )

  private def config2Json(config: ConfigObject): Map[String, Json] = {
    val jsonStr = config.render(ConfigRenderOptions.concise)
    parser.parse(jsonStr).toTry.get.asObject.get.toMap
  }
}

case class AppConfig(
  settings: Settings,
  genesis: Genesis
)
