package scorex.core.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import scorex.core.settings.RESTApiSettings
import scorex.crypto.hash.{Blake2b256, Digest}

import scala.concurrent.{Await, Future}

trait ApiRoute extends Directives {
  val settings: RESTApiSettings
  val context: ActorRefFactory
  val route: Route

  implicit lazy val timeout: Timeout = Timeout(settings.timeout)

  def actorRefFactory: ActorRefFactory = context

  def getJsonRoute(fn: Future[ScorexApiResponse]): Route =
    jsonRoute(Await.result(fn, settings.timeout), get)

  def getJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, get)

  def postJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, post)

  def postJsonRoute(fn: Future[ScorexApiResponse]): Route = jsonRoute(Await.result(fn, settings.timeout), post)

  def deleteJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, delete)

  def deleteJsonRoute(fn: Future[ScorexApiResponse]): Route = jsonRoute(Await.result(fn, settings.timeout), delete)

  protected def jsonRoute(fn: ScorexApiResponse, method: Directive0): Route = method {
    val resp = fn match {
      case SuccessApiResponse(js) =>
        complete(HttpEntity(ContentTypes.`application/json`, js.spaces2))
      case ApiException(e) =>
        complete(StatusCodes.InternalServerError -> e.getMessage)
      case err@ApiError(msg, code) =>
        complete(code -> msg)
    }
    withCors(resp)
  }

  def withCors(fn: => Route): Route = {
    if (settings.corsAllowed) respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else fn
  }

  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("api_key") { keyOpt =>
      if (isValid(keyOpt)) route
      else withCors(complete(StatusCodes.Forbidden -> ApiError.apiKeyNotValid.message))
    }
  }

  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[Digest] = keyOpt.map(Blake2b256(_))
    (settings.apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected.toCharArray sameElements passed
      case _ => false
    }
  }
}