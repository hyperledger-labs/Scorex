package scorex.settings

import java.io.File
import java.net.InetSocketAddress

import io.circe.Json
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Settings
  */

trait Settings extends ScorexLogging {

  val filename: String

  lazy val settingsJSON: Map[String, Json] = Try {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    Json.fromString(jsonString)
  }.recoverWith { case t =>
    Try {
      val jsonString = scala.io.Source.fromURL(getClass.getResource(s"/$filename")).mkString
      Json.fromString(jsonString)
    }
  }.toOption
    .flatMap(_.asObject)
    .map(_.toMap)
    .getOrElse {
      log.error(s"Unable to read $filename or not a JSON map there, closing")
      //catch error?
      System.exit(10)
      Map()
    }

  private def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
  }

  lazy val dataDirOpt = {
    val res = settingsJSON.get("dataDir").flatMap(_.asString)
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  //p2p
  lazy val DefaultPort = 9084

  lazy val p2pSettings = settingsJSON("p2p").asObject.get.toMap

  lazy val nodeNonce: Long = (Random.nextInt(1000) + 1000) * Random.nextInt(1000) + Random.nextInt(1000)

  lazy val nodeName = p2pSettings.get("nodeName").flatMap(_.asString)
    .getOrElse(Random.nextPrintableChar().toString + nodeNonce)

  lazy val localOnly = p2pSettings.get("localOnly").flatMap(_.asBoolean).getOrElse(false)

  lazy val knownPeers = Try {
    p2pSettings.get("knownPeers").flatMap(_.asArray).map(_.flatMap(_.asString)).map(_.map { addr =>
      val addrParts = addr.split(":")
      val port = if (addrParts.size == 2) addrParts(1).toInt else DefaultPort
      new InetSocketAddress(addrParts(0), port)
    })
  }.toOption.flatten.getOrElse(Seq[InetSocketAddress]())

  lazy val bindAddress = p2pSettings.get("bindAddress").flatMap(_.asString).getOrElse(DefaultBindAddress)
  lazy val maxConnections = p2pSettings.get("maxConnections")
    .flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMaxConnections)
  lazy val connectionTimeout = p2pSettings.get("connectionTimeout")
    .flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultConnectionTimeout)
  lazy val upnpEnabled = p2pSettings.get("upnp").flatMap(_.asBoolean).getOrElse(true)
  lazy val upnpGatewayTimeout = p2pSettings.get("upnpGatewayTimeout").flatMap(_.asNumber).flatMap(_.toInt)
  lazy val upnpDiscoverTimeout = p2pSettings.get("upnpDiscoverTimeout").flatMap(_.asNumber).flatMap(_.toInt)
  lazy val port = p2pSettings.get("port").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultPort)
  lazy val declaredAddress = p2pSettings.get("myAddress").flatMap(_.asString)

  //p2p settings assertions
  assert(!(localOnly && upnpEnabled), "Both localOnly and upnp enabled")
  //todo: localOnly & declaredAddress

  lazy val rpcPort = settingsJSON.get("rpcPort").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultRpcPort)
  lazy val rpcAllowed: Seq[String] = settingsJSON.get("rpcAllowed").flatMap(_.asArray.map(_.flatMap(_.asString))).getOrElse(DefaultRpcAllowed.split(""))

  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)
  lazy val historySynchronizerTimeout: FiniteDuration = settingsJSON.get("historySynchronizerTimeout").flatMap(_.asNumber).flatMap(_.toInt)
    .map(x => x.seconds).getOrElse(DefaultHistorySynchronizerTimeout)

  lazy val blockGenerationDelay: FiniteDuration = settingsJSON.get("blockGenerationDelay").flatMap(_.asNumber).flatMap(_.toLong)
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultBlockGenerationDelay)

  lazy val mininigThreads: Int = settingsJSON.get("mininigThreads").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMiningThreads)

  lazy val walletDirOpt = settingsJSON.get("walletDir").flatMap(_.asString)
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  lazy val walletPassword = settingsJSON.get("walletPassword").flatMap(_.asString).getOrElse {
    println("Please type your wallet password")
    scala.io.StdIn.readLine()
  }

  lazy val walletSeed = settingsJSON.get("walletSeed").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)

  lazy val apiKeyHash = settingsJSON.get("apiKeyHash").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)

  //todo: move to application.conf
  lazy val genesisTimestamp: Long = settingsJSON.get("genesisTimestamp").flatMap(_.asNumber).flatMap(_.toLong).getOrElse(DefaultGenesisTimestamp)

  //NETWORK
  private val DefaultMaxConnections = 20
  private val DefaultConnectionTimeout = 60
  private val DefaultBindAddress = "127.0.0.1"

  val MaxBlocksChunks = 10

  //API
  lazy val corsAllowed = settingsJSON.get("cors").flatMap(_.asBoolean).getOrElse(false)

  private val DefaultRpcPort = 9085
  private val DefaultRpcAllowed = "127.0.0.1"

  private val DefaultBlockGenerationDelay: FiniteDuration = 1.second
  private val DefaultHistorySynchronizerTimeout: FiniteDuration = 30.seconds
  private val DefaultMiningThreads: Int = 1

  private val DefaultGenesisTimestamp: Long = 1460952000000L
}
