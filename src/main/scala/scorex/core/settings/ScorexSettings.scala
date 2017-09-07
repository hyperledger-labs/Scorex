package scorex.core.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

trait ScorexSettings extends Settings {
  val networkSettings: NetworkSettings
}


object ScorexSettings extends SettingsParser {

  val configPath: String = "scorex"

  def read(userConfigPath: Option[String]): ScorexSettings = {
    fromConfig(readConfigFromPath(userConfigPath: Option[String], configPath))
  }

  def fromConfig(config: Config): ScorexSettings = {
    val p2pSettingsParsed = config.as[NetworkSettings](s"$configPath.p2psettings")

    new ScorexSettings {
      override val networkSettings: NetworkSettings = p2pSettingsParsed
    }
  }

}
