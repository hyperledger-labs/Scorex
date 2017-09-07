package scorex.core.settings

import java.io.File
import java.net.InetSocketAddress

import io.circe.Json
import io.circe.parser.parse
import scorex.core.app.Version
import scorex.core.transaction.box.proposition.Constants25519._
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
  * Settings
  */
trait Settings extends ScorexLogging {

  def settingsFromFile(filename: String): Map[String, Json] = Try {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    parse(jsonString).right.get
  }.recoverWith { case t =>
    Try {
      val jsonString = scala.io.Source.fromURL(getClass.getResource(s"/$filename")).mkString
      parse(jsonString).right.get
    }
  }.toOption.flatMap(_.asObject).map(_.toMap).getOrElse {
    log.error(s"Unable to read $filename or not a JSON map there, closing")
    //catch error?
    System.exit(10)
    Map()
  }

  def settingsJSON: Map[String, Json]

  private def directoryEnsuring(dirPath: String): Boolean = {
    val f = new java.io.File(dirPath)
    f.mkdirs()
    f.exists()
  }

  private def folderOpt(settingName: String) = {
    val res = settingsJSON.get(settingName).flatMap(_.asString)
    res.foreach(folder => new File(folder).mkdirs())
    require(res.isEmpty || new File(res.get).exists())
    res
  }

  lazy val dataDirOpt = folderOpt("dataDir")

  lazy val logDirOpt = folderOpt("logDir")

  //There are different bind addresses
  lazy val bindAddress: String = ???

  lazy val rpcPort = settingsJSON.get("rpcPort").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultRpcPort)

  lazy val blockGenerationDelay: FiniteDuration = settingsJSON.get("blockGenerationDelay").flatMap(_.asNumber).flatMap(_.toLong)
    .map(x => FiniteDuration(x, MILLISECONDS)).getOrElse(DefaultBlockGenerationDelay)

  lazy val mininigThreads: Int = settingsJSON.get("mininigThreads").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(DefaultMiningThreads)

  lazy val walletDirOpt = settingsJSON.get("walletDir").flatMap(_.asString)
    .ensuring(pathOpt => pathOpt.forall(directoryEnsuring))

  lazy val walletPassword = settingsJSON.get("walletPassword").flatMap(_.asString).getOrElse {
    scala.io.StdIn.readLine()
  }

  lazy val walletSeed = settingsJSON.get("walletSeed").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)
    .getOrElse {
      val generated = scorex.utils.Random.randomBytes(PrivKeyLength)
      log.warn("No wallet seed provided: generated new one:" + Base58.encode(generated))
      generated
    }

  lazy val apiKeyHash = settingsJSON.get("apiKeyHash").flatMap(_.asString).flatMap(s => Base58.decode(s).toOption)

  lazy val corsAllowed = settingsJSON.get("cors").flatMap(_.asBoolean).getOrElse(false)

  lazy val isTestnet = settingsJSON.get("testnet").flatMap(_.asBoolean).getOrElse(false)

  //NETWORK
  private val DefaultMaxConnections = 20
  private val DefaultConnectionTimeout = 60
  private val DefaultBindAddress = "127.0.0.1"

  private val DefaultNetworkChunkSize = 100

  //API

  private val DefaultRpcPort = 9085

  private val DefaultBlockGenerationDelay: FiniteDuration = 1.second
  private val DefaultMiningThreads: Int = 1

  //APPLICATION DATA
  lazy val agentName: String = settingsJSON.get("agent").flatMap(_.asString)
    .getOrElse(Random.alphanumeric.take(16).mkString)

  lazy val appVersion: Version = settingsJSON.get("version").flatMap(_.asArray)
    .map(_.flatMap(_.asNumber.flatMap(_.toByte))).map(_.toArray)
    .map(arr => Version(arr(0), arr(1), arr(2)))
    .getOrElse(Version(0, 0, 1))
}

object Settings {


  val VersionNumbers = 3 //a version is about 3 numbers e.g. 1.0.1
}
