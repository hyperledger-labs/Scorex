package io.scalac.elm.config

import io.circe.Json
import scorex.core.settings.Settings

class ElmSettings(filename: String) extends Settings {
  override def settingsJSON: Map[String, Json] = settingsFromFile(filename)
}
