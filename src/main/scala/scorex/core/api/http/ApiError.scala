package scorex.core.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Route

object ApiError extends JsonRoute(StatusCodes.InternalServerError) {
  def apply(s: String): Route = withString(s)
  def apply(e: Throwable): Route = withString(e.getMessage)
  def apply(causes: Seq[Throwable]): Route = withString(mkString(causes))
  def mkString(causes: Seq[Throwable]): String = causes.map(_.getMessage).mkString(", ")

  object InvalidJson extends JsonStringRoute(StatusCodes.BadRequest, "invalid.json")
  object BadRequest extends JsonStringRoute(StatusCodes.BadRequest, "bad.request")
  object ApiKeyNotValid extends JsonStringRoute(StatusCodes.Forbidden, "invalid.api-key")
  object NotExists extends JsonStringRoute(StatusCodes.NotFound, "not-found")

  class JsonStringRoute(statusCode: StatusCode, reason: String) extends JsonRoute(statusCode) {
    override def defaultMessage: String = reason
  }
}
