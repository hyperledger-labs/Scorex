package scorex.core.settings

import java.io.File
import java.net.InetSocketAddress

import com.github.swagger.akka.model.Info
import com.typesafe.config.{Config, ConfigFactory}
import scorex.core.utils.{ByteStr, ScorexLogging}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.util.Random

case class RESTApiSettings(bindAddress: String,
                           port: Int,
                           apiKeyHash: Option[String],
                           corsAllowed: Boolean,
                           timeout: FiniteDuration,
                           swaggerInfo: Info)

case class NetworkSettings(nodeName: String,
                           nodeNonce: Option[Long] = Some(new Random().nextLong()),
                           addedMaxDelay: Option[FiniteDuration],
                           networkChunkSize: Int,
                           localOnly: Boolean,
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: String,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[FiniteDuration],
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           port: Int,
                           declaredAddress: Option[String],
                           handshakeTimeout: FiniteDuration,
                           deliveryTimeout: FiniteDuration,
                           maxDeliveryChecks: Int,
                           appVersion: String,
                           agentName: String,
                           maxPacketLen: Int,
                           maxInvObjects: Int,
                           syncInterval: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           syncStatusRefresh: FiniteDuration,
                           controllerTimeout: Option[FiniteDuration]
                          )

case class MinerSettings(offlineGeneration: Boolean,
                         targetBlockDelay: FiniteDuration,
                         blockGenerationDelay: FiniteDuration)

case class WalletSettings(seed: ByteStr,
                          password: String,
                          walletDir: File)

case class ScorexSettings(dataDir: File,
                          logDir: File,
                          network: NetworkSettings,
                          restApi: RESTApiSettings,
                          miner: MinerSettings,
                          wallet: WalletSettings)


object ScorexSettings extends ScorexLogging {

  protected val configPath: String = "scorex"

  def readConfigFromPath(userConfigPath: Option[String], configPath: String): Config = {
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
          .withFallback(cfg) // user-supplied config
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference()) // "src/main/resources/reference.conf"
          .resolve()
    }

    config
  }

  def read(userConfigPath: Option[String]): ScorexSettings = {
    fromConfig(readConfigFromPath(userConfigPath, configPath))
  }

  def fromConfig(config: Config): ScorexSettings = {
    config.as[ScorexSettings](configPath)
  }
}
