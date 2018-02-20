package scorex.core.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Printer
import scorex.core.utils.ActorHelper

import scala.concurrent.{Await, Future}

trait ApiRoute extends ApiDirectives with ActorHelper with FailFastCirceSupport with PredefinedFromEntityUnmarshallers {

  def context: ActorRefFactory
  def route: Route

  //TODO: should we move it to the settings?
  override val apiKeyHeaderName: String = "api_key"

  implicit val printer: Printer = Printer.spaces2.copy(dropNullValues = true)
  implicit lazy val timeout: Timeout = Timeout(settings.timeout)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
  def getJsonRoute(fn: Future[ScorexApiResponse]): Route =
    jsonRoute(Await.result(fn, settings.timeout), get)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
  def getJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, get)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
  def postJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, post)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
  def postJsonRoute(fn: Future[ScorexApiResponse]): Route = jsonRoute(Await.result(fn, settings.timeout), post)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
  def deleteJsonRoute(fn: ScorexApiResponse): Route = jsonRoute(fn, delete)

  @deprecated("Use circe json support for answering with json", "31 January 2018")
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


}