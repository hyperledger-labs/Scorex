package scorex.core.api.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._

object ApiResponse extends JsonRoute(StatusCodes.OK) {
  val OK: ApiResponse.type = ApiResponse
  def apply(result: Either[Throwable, Json]): Route = result.fold(ApiError.apply, apply)
  def apply(keyValues: (String, Json)*): Route = withJson(Map(keyValues: _*).asJson)
  def apply(keyValue: (String, String)): Route = withJson(Map(keyValue).asJson)
}


