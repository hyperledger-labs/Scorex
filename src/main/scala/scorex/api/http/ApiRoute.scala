package scorex.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import io.circe.Json
import scorex.app.Application
import scorex.crypto.hash.{CryptographicHash, SecureCryptographicHash}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ApiRoute extends Directives with CommonApiFunctions {
  val application: Application
  val context: ActorRefFactory
  val route: Route

  implicit val timeout = Timeout(5.seconds)

  lazy val corsAllowed = application.settings.corsAllowed
  lazy val apiKeyHash = application.settings.apiKeyHash

  def actorRefFactory: ActorRefFactory = context

  def getJsonRoute(fn: Future[Json]): Route =
    jsonRoute(Await.result(fn, timeout.duration), get)

  def getJsonRoute(fn: Json): Route = jsonRoute(fn, get)

  def postJsonRoute(fn: Json): Route = jsonRoute(fn, post)

  def postJsonRoute(fn: Future[Json]): Route = jsonRoute(Await.result(fn, timeout.duration), post)

  def deleteJsonRoute(fn: Json): Route = jsonRoute(fn, delete)

  def deleteJsonRoute(fn: Future[Json]): Route = jsonRoute(Await.result(fn, timeout.duration), delete)

  private def jsonRoute(fn: Json, method: Directive0): Route = method {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toString))
    withCors(resp)
  }

  def withCors(fn: => Route): Route = {
    if (corsAllowed) respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else fn
  }

  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("api_key") { case keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, ApiError.apiKeyNotValid.toString()))
    }
  }

  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[CryptographicHash#Digest] = keyOpt.map(SecureCryptographicHash(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
    }
  }

}