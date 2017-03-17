package scorex.core.api.http

import io.circe.Json
import io.circe.syntax._

trait ScorexApiResponse {

  val success: Boolean
  val data: Json

  def toJson: Json = Map(
    "success" -> success.asJson,
    "data" -> data
  ).asJson

}