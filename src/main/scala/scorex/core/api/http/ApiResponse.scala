package scorex.core.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.Future
import scala.language.implicitConversions

class ApiResponse(statusCode: StatusCode) {
  def apply(result: Future[Json]): Route = Directives.onSuccess(result)(complete)
  def apply(result: Json): Route = withJson(result)

  def defaultRoute: Route = withString(defaultMessage)
  def defaultMessage: String = statusCode.reason

  def withString(s: String): Route = complete(s.asJson)
  def withJson(result: Json): Route = complete(result)

  def complete(result: Json): Route = {
    Directives.complete(statusCode.intValue() -> HttpEntity(ContentTypes.`application/json`, result.spaces2))
  }
}

object ApiResponse {
  implicit def toRoute(jsonRoute: ApiResponse): Route = jsonRoute.defaultRoute

  def apply(result: Json): Route = OK(result)
  def apply(result: Future[Json]): Route = OK(result)
  def apply(result: Either[Throwable, Json]): Route = result.fold(ApiError.apply, apply)
  def apply(keyValues: (String, Json)*): Route = apply(Map(keyValues: _*).asJson)
  def apply(keyValue: (String, String)): Route = apply(Map(keyValue).asJson)

  object OK extends ApiResponse(StatusCodes.OK)
}
