package examples.hybrid.mining

import io.circe.syntax._
import scorex.core.settings.Settings

import scala.concurrent.duration._

trait MiningSettings extends Settings with MiningConstants {
  lazy val targetBlockDelay: Long = settingsJSON.get("targetBlockDelayMillis").flatMap(_.asNumber).flatMap(_.toLong)
    .getOrElse(DefaultTargetBlockDelayMillis)

  lazy val RParamX10: Long = settingsJSON.get("RparamX10").flatMap(_.asNumber).flatMap(_.toLong)
    .getOrElse(DefaultRParamX10)

  lazy val offlineGeneration = settingsJSON.get("offlineGeneration").flatMap(_.asBoolean).getOrElse(false)

  lazy val posAttachmentSize = settingsJSON.get("posAttachmentSize").flatMap(_.asNumber).flatMap(_.toInt)
    .getOrElse(DefaulPtosAttachmentSize)

  val DefaulPtosAttachmentSize = 1024
  val DefaultTargetBlockDelayMillis = 3000L
  val DefaultRParamX10 = 8L

  override def toString: String = (Map("BlockDelay" -> targetBlockDelay.asJson) ++
    settingsJSON.map(s => s._1 -> s._2)).asJson.spaces2
}
