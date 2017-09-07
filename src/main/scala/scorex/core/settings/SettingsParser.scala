package scorex.core.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import scorex.core.utils.ScorexLogging

trait SettingsParser extends ScorexLogging {

  protected def readConfigFromPath(userConfigPath: Option[String], configPath: String): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath(configPath)) {
          throw new Error("Malformed configuration file was provided! Aborting!")
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }

    config
  }

}
