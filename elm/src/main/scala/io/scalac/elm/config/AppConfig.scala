package io.scalac.elm.config

import com.typesafe.config.ConfigFactory

object AppConfig {
  def load(): AppConfig = {
    val config = ConfigFactory.load().getConfig("app")

    AppConfig(
      settings = new ElmSettings(config.getString("settings-location")),
      N = config.getInt("N")
    )
  }
}

case class AppConfig(
  settings: ElmSettings,
  N: Int
)
