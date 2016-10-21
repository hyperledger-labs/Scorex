package io.scalac.elm.config

import com.typesafe.config.{ConfigFactory, ConfigObject, ConfigRenderOptions}
import io.circe.{Json, parser}
import scorex.core.settings.Settings

object AppConfig {
  def load(): AppConfig = {
    val config = ConfigFactory.load().getConfig("app")

    AppConfig(
      settings = new Settings {
        val settingsJSON = config2Json(config.getObject("scorex-setttings"))
      },
      N = config.getInt("N")
    )
  }

  private def config2Json(config: ConfigObject): Map[String, Json] = {
    val jsonStr = config.render(ConfigRenderOptions.concise)
    parser.parse(jsonStr).toTry.get.asObject.get.toMap
  }
}

case class AppConfig(
  settings: Settings,
  N: Int
)
