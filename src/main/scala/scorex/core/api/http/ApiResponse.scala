package scorex.core.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode}
import akka.http.scaladsl.model.StatusCodes._
import io.circe.{Json, Printer}
import io.circe.syntax._

import scala.language.implicitConversions

class ApiResponse(val status: StatusCode) extends AnyVal {
  def json: ToResponseMarshallable = withJson(status.reason.asJson)
  def withJsonStr(s: String): ToResponseMarshallable = withJson(s.asJson)
  def withJson(data: Json): ToResponseMarshallable = {
    status.intValue() -> HttpEntity(ContentTypes.`application/json`, data.spaces2)
  }
}

object ApiResponse {

  implicit def apply(status: StatusCode): ApiResponse = new ApiResponse(status)
  implicit val printer: Printer = Printer.spaces2.copy(dropNullValues = true)

  def apply(data: Json): ToResponseMarshallable = fromJson(data)
  def apply(keyValues: (String, Json)*): ToResponseMarshallable = fromJson(Map(keyValues: _*).asJson)
  def apply(keyValue: (String, String)): ToResponseMarshallable = fromJson(Map(keyValue).asJson)
  def apply(result: Either[Throwable, Json]): ToResponseMarshallable = result.fold(fromError, fromJson)
  def apply(e: Throwable): ToResponseMarshallable = fromError(e)

  def fromJson(data: Json): ToResponseMarshallable = OK.withJson(data)
  def fromError(e: Throwable): ToResponseMarshallable = InternalServerError.withJsonStr(e.getMessage)
  def fromErrors(causes: Seq[Throwable]): ToResponseMarshallable = InternalServerError.withJsonStr(mkString(causes))
  def mkString(causes: Seq[Throwable]): String = causes.map(_.getMessage).mkString(", ")

  def ok: ToResponseMarshallable = OK.json
  def unknown: ToResponseMarshallable = InternalServerError.withJsonStr("unknown")
  def wrongJson: ToResponseMarshallable = BadRequest.withJsonStr("invalid.json")
  def badRequest: ToResponseMarshallable = BadRequest.withJsonStr("bad.request")
  def apiKeyNotValid: ToResponseMarshallable = Forbidden.withJsonStr("invalid.api-key")
  def notExists: ToResponseMarshallable = NotFound.withJsonStr("not-found")

}

