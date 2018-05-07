package scorex.core.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.server.{Directives, Route}
import io.circe.Json
import io.circe.syntax._
import scala.language.implicitConversions

import scala.concurrent.Future

class JsonRoute(statusCode: StatusCode) {
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

object JsonRoute {
  implicit def toRoute(jsonRoute: JsonRoute): Route = jsonRoute.defaultRoute
}
