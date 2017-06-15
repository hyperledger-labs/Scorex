package scorex.core.api.http

import io.circe.Json

import scala.util.{Failure, Success, Try}

object ApiTry {

  def apply(f: => Json): ScorexApiResponse =Try {
    f
  } match {
    case Success(r) => SuccessApiResponse(r)
    case Failure(e) =>
      e.printStackTrace()
      ApiException(e)
  }

}
