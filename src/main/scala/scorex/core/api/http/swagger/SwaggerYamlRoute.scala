package scorex.core.api.http.swagger

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Route
import com.github.swagger.akka.CustomMediaTypes
import scorex.core.api.http.ApiRoute
import scorex.core.settings.RESTApiSettings

class SwaggerYamlRoute(swaggerYaml: String, override val settings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends ApiRoute {

  override val route: Route = {
    path("api-docs" / "swagger.yaml") {
      get {
        complete(HttpEntity(CustomMediaTypes.`text/vnd.yaml`, swaggerYaml))
      }
    }
  }

}

