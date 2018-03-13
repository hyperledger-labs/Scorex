package scorex.core.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import io.circe._
import io.circe.syntax._

import scala.language.implicitConversions

case class ApiError(message: String, code: StatusCode) extends ScorexApiResponse {
  override val data: Json = message.asJson
  def asHttpResponse: ToResponseMarshallable = code -> message
}

case class ApiException(e: Throwable) extends ScorexApiResponse {
  override val code: StatusCode = StatusCodes.InternalServerError
  override val data: Json = e.getMessage.asJson
  def asHttpResponse: ToResponseMarshallable = StatusCodes.InternalServerError -> e.getMessage
}

object ApiError {
  val unknown: ApiError = ApiError("unknown", StatusCodes.InternalServerError)
  val wrongJson: ApiError = ApiError("invalid.json", StatusCodes.BadRequest)
  val apiKeyNotValid: ApiError = ApiError("invalid.api-key", StatusCodes.Forbidden)
  val notExists: ApiError = ApiError("not-found", StatusCodes.NotFound)
}
