package scorex.core.api.http

import akka.http.scaladsl.server.{Directives, ExceptionHandler}

import scala.util.control.NonFatal

object ApiErrorHandler {

  implicit val exceptionHandler: ExceptionHandler = ExceptionHandler {
    case NonFatal(e) => ApiError(e)
  }

}
