package scorex.core.api.http.swagger

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import scorex.core.api.http.ApiRoute
import scorex.core.settings.RESTApiSettings

class SwaggerConfigRoute(swaggerConf: String, override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  override val route: Route = {
    (get & path("api-docs" / "swagger.conf")) {
      complete(HttpEntity(ContentTypes.`application/json`, swaggerConf))
    }
  }
}

