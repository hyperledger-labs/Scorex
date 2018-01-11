package scorex.core.api.http

import akka.http.scaladsl.model.StatusCode
import io.circe.Json
import io.circe.syntax._

trait ScorexApiResponse {
  def code: StatusCode
  def data: Json

  def toJson: Json = data.asJson

}