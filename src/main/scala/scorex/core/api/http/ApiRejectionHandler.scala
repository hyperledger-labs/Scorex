package scorex.core.api.http

import akka.http.scaladsl.server._

object ApiRejectionHandler {

  implicit val rejectionHandler: RejectionHandler = RejectionHandler.newBuilder()
    .handleAll[SchemeRejection] { rejections =>
      val schemes = rejections.map(_.supported).mkString(", ")
      ApiError.BadRequest.complete(s"Uri scheme not allowed, supported schemes: $schemes")
    }
    .handle {
      case AuthorizationFailedRejection =>
        ApiError.Forbidden.complete("The supplied authentication is not authorized to access this resource")
    }
    .handle {
      case MalformedRequestContentRejection(msg, _) =>
        ApiError.BadRequest.complete("The request content was malformed:\n" + msg)
    }
    .handle {
      case InvalidOriginRejection(allowedOrigins) =>
        ApiError.Forbidden.complete(s"Allowed `Origin` header values: ${allowedOrigins.mkString(", ")}")
    }
    .handle {
      case MissingQueryParamRejection(paramName) =>
        ApiError.NotExists.complete(s"Request is missing required query parameter '$paramName'")
    }
    .handle {
      case RequestEntityExpectedRejection =>
        ApiError.BadRequest.complete("Request entity expected but not supplied")
    }
    .handle { case ValidationRejection(msg, _) => ApiError.BadRequest.complete(msg) }
    .handle { case x => ApiError.InternalError.complete(s"Unhandled rejection: $x") }
    .handleNotFound { ApiError.NotExists.complete("The requested resource could not be found.") }
    .result()
}
