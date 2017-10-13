package examples.hybrid.mining

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scorex.core.ModifierId
import scorex.core.settings._
import scorex.core.settings.ScorexSettings.readConfigFromPath

import scala.concurrent.duration._

case class HybridSettings(mining: HybridMiningSettings,
                          scorexSettings: ScorexSettings)

case class HybridMiningSettings(offlineGeneration: Boolean,
                                targetBlockDelay: FiniteDuration,
                                blockGenerationDelay: FiniteDuration,
                                posAttachmentSize: Int,
                                rParamX10: Int,
                                initialDifficulty: BigInt) {
  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val GenesisParentId = ModifierId @@ Array.fill(32)(1: Byte)
}

object HybridSettings {
  def read(userConfigPath: Option[String]): HybridSettings = {
    fromConfig(readConfigFromPath(userConfigPath, configPath))
  }

  implicit val networkSettingsValueReader: ValueReader[HybridSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  val configPath: String = "hybrid"

  private def fromConfig(config: Config): HybridSettings = {
    val miningSettings = config.as[HybridMiningSettings](s"$configPath.mining")
    val scorexSettings = config.as[ScorexSettings](configPath)
    HybridSettings(miningSettings, scorexSettings)
  }
}

