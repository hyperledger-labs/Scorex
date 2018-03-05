package scorex.core.api.http.swagger

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import scorex.core.api.http.{ApiRoute, SuccessApiResponse}
import scorex.core.settings.RESTApiSettings

class SwaggerConfigRoute(swaggerConf: String, override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  override val route: Route = {
    path("api-docs" / "swagger.conf") {
      jsonRoute(SuccessApiResponse(Json.fromString(swaggerConf)), get)
    }
  }
}

