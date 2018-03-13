package scorex.core.api.http

import akka.http.scaladsl.server.{Directives, Route}

import scala.util.{Failure, Success, Try}

object ApiTry {

  def apply(f: => Route): Route = Try {
    f
  } match {
    case Success(r) => r
    case Failure(e) => e.printStackTrace(); Directives.complete(ApiException(e))
  }

}
