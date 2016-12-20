package examples.hybrid.mining

import scorex.core.settings.Settings

trait MiningSettings extends Settings with MiningConstants {

  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)
}
