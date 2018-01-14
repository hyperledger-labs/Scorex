package scorex.core.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import io.circe.Json

case class SuccessApiResponse(data: Json) extends ScorexApiResponse {
  override val code: StatusCode = StatusCodes.OK
}
