package scorex.core.api.http

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.language.implicitConversions

case class ApiError(message: String) extends ScorexApiResponse {

  override val success: Boolean = false

  override val data: Json = message.asJson
}

case class ApiException(e: Throwable) extends ScorexApiResponse {
  override val success: Boolean = false
  override val data: Json = e.getMessage.asJson
}

object ApiError {

  val unknown: ApiError = ApiError("unknown")
  val wrongJson: ApiError = ApiError("invalid.json")
  val apiKeyNotValid: ApiError = ApiError("invalid.apikey")
  val blockNotExists: ApiError = ApiError("Block does not exist")
  val transactionNotExists: ApiError = ApiError("Transaction does not exist")
}
