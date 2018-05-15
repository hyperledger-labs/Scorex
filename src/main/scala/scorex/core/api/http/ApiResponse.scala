package scorex.core.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import io.circe.{Encoder, Json}
import io.circe.syntax._

import scala.concurrent.Future
import scala.language.implicitConversions

/** Bunch of methods to wrap json result to route with the given status code, completing it.
  * When receiving `Null` json, then complete route with `404 not found` status code.
  */
class ApiResponse(statusCode: StatusCode) {

  def apply(result: Json): Route = withJson(result)
  def apply[R: Encoder](result: R): Route = withJson(result)
  def apply[R: Encoder](result: Future[R]): Route = Directives.onSuccess(result)(withJson)
  def apply(result: Future[Json]): Route = Directives.onSuccess(result)(withJson)

  def defaultRoute: Route = withString(defaultMessage)
  def defaultMessage: String = statusCode.reason

  def withString(s: String): Route = complete(s.asJson)

  def withJson[R](result: R)(implicit encoder: Encoder[R]): Route = complete(encoder(result))

  def complete(result: Json): Route = result match {
    case json if json.isNull => ApiError.NotExists
    case _ => Directives.complete(statusCode.intValue() -> HttpEntity(ContentTypes.`application/json`, result.spaces2))
  }
}

object ApiResponse {
  implicit def toRoute(jsonRoute: ApiResponse): Route = jsonRoute.defaultRoute

  def apply[R: Encoder](result: R): Route = OK(result)
  def apply[R: Encoder](result: Future[R]): Route = OK(result)
  def apply(result: Either[Throwable, Json]): Route = result.fold(ApiError.apply, OK.apply)
  def apply(keyValues: (String, Json)*): Route = apply(Map(keyValues: _*).asJson)
  def apply(keyValue: (String, String)): Route = apply(Map(keyValue).asJson)

  object OK extends ApiResponse(StatusCodes.OK)
}
