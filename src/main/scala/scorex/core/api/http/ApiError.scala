package scorex.core.api.http

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}

import scala.language.implicitConversions

case class ApiError(statusCode: StatusCode, reason: String = "") {

  def apply(detail: String): Route = complete(detail)
  def defaultRoute: Route = complete()

  def complete(detail: String = ""): Route = {
    val nonEmptyReason = if (reason.isEmpty) statusCode.reason else reason
    val result = if (detail.isEmpty) nonEmptyReason else s"$nonEmptyReason $detail"
    val httpEntity = HttpEntity(ContentTypes.`application/json`, result)
    Directives.complete(statusCode.intValue() -> httpEntity)
  }
}

object ApiError {
  def apply(s: String): Route = InternalError(s)
  def apply(e: Throwable): Route = InternalError(safeMessage(e))
  def apply(causes: Seq[Throwable]): Route = InternalError(mkString(causes))
  def mkString(causes: Seq[Throwable]): String = causes.map(safeMessage).mkString(", ")
  private def safeMessage(e: Throwable): String = Option(e.getMessage).getOrElse(e.toString)

  implicit def toRoute(error: ApiError): Route = error.defaultRoute

  object InternalError extends ApiError(StatusCodes.InternalServerError, "internal.error")
  object InvalidJson extends ApiError(StatusCodes.BadRequest, "invalid.json")
  object BadRequest extends ApiError(StatusCodes.BadRequest, "bad.request")
  object ApiKeyNotValid extends ApiError(StatusCodes.Forbidden, "invalid.api-key")
  object NotExists extends ApiError(StatusCodes.NotFound, "not-found")

}
