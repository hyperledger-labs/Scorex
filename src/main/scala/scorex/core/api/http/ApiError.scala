package scorex.core.api.http

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.language.implicitConversions

case class ApiError(error: Int, message: String) extends ScorexApiResponse {

  override val success: Boolean = false

  override val data: Json = Map(
    "error-code" -> error.asJson,
    "message" -> message.asJson
  ).asJson
}

case class ApiException(e: Throwable) extends ScorexApiResponse {
  override val success: Boolean = false
  override val data: Json = e.getMessage.asJson
}

object ApiError {

  val unknown: ApiError = ApiError(0, "Error is unknown")
  val wrongJson: ApiError = ApiError(1, "Failed to parse json message")
  val apiKeyNotValid: ApiError = ApiError(2, "Provided API key is not correct")
  val blockNotExists: ApiError = ApiError(3, "Block does not exist")
  val transactionNotExists: ApiError = ApiError(4, "Transaction does not exist")
}
