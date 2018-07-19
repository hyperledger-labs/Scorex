package examples.hybrid.mining

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scorex.core.bytesToId
import scorex.core.settings.ScorexSettings.readConfigFromPath
import scorex.core.settings._
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration._

case class HybridSettings(mining: HybridMiningSettings,
                          walletSettings: WalletSettings,
                          scorexSettings: ScorexSettings)

case class WalletSettings(seed: String,
                          password: String,
                          walletDir: File)

case class HybridMiningSettings(offlineGeneration: Boolean,
                                targetBlockDelay: FiniteDuration,
                                blockGenerationDelay: FiniteDuration,
                                posAttachmentSize: Int,
                                rParamX10: Int,
                                initialDifficulty: BigInt) {
  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val GenesisParentId = bytesToId(Array.fill(32)(1: Byte))
}

object HybridSettings extends ScorexLogging with SettingsReaders {
  def read(userConfigPath: Option[String]): HybridSettings = {
    fromConfig(readConfigFromPath(userConfigPath, "scorex"))
  }

  implicit val networkSettingsValueReader: ValueReader[HybridSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  private def fromConfig(config: Config): HybridSettings = {
    log.info(config.toString)
    val walletSettings = config.as[WalletSettings]("scorex.wallet")
    val miningSettings = config.as[HybridMiningSettings]("scorex.miner")
    val scorexSettings = config.as[ScorexSettings]("scorex")
    HybridSettings(miningSettings, walletSettings, scorexSettings)
  }
}

