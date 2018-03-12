package scorex.core.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCode, StatusCodes}
import io.circe.Json
import io.circe.syntax._

case class SuccessApiResponse(data: Json) extends ScorexApiResponse {
  override val code: StatusCode = StatusCodes.OK
  def asHttpResponse: ToResponseMarshallable = HttpEntity(ContentTypes.`application/json`, data.spaces2)
}

object SuccessApiResponse {

  def apply(keyValues: (String, Json)*): SuccessApiResponse = {
    SuccessApiResponse(Map(keyValues: _*).asJson)
  }

  def apply(keyValue: (String, String)): SuccessApiResponse = {
    SuccessApiResponse(Map(keyValue).asJson)
  }
}
