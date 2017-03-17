package scorex.core.api.http

import io.circe.Json

case class SuccessApiResponse(data: Json) extends ScorexApiResponse {
  override val success: Boolean = true
}
