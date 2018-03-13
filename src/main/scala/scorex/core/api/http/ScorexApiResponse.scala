package scorex.core.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCode
import io.circe.Json
import scala.language.implicitConversions

trait ScorexApiResponse {
  def code: StatusCode
  def data: Json

  def toJson: Json = data
  def asHttpResponse: ToResponseMarshallable
}

object ScorexApiResponse {
  implicit def toHttpResponse(r: ScorexApiResponse): ToResponseMarshallable  = r.asHttpResponse
}