package scorex.core.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives
import scorex.core.api.http.swagger.SwaggerConfigRoute
import scorex.core.settings.RESTApiSettings

case class CompositeHttpService(system: ActorSystem, routes: Seq[ApiRoute], settings: RESTApiSettings, swaggerConf: String)
  extends CorsHandler {

  implicit val actorSystem: ActorSystem = system

  val swaggerService: SwaggerConfigRoute = new SwaggerConfigRoute(swaggerConf: String, settings: RESTApiSettings)

  val redirectToSwagger: Route = path("" | "/") {
    redirect("/swagger", StatusCodes.PermanentRedirect)
  }

  val compositeRoute: Route =
    routes.map(_.route).reduceOption(_ ~ _).getOrElse(RouteDirectives.reject) ~
      swaggerService.route ~
      path("swagger")(getFromResource("swagger-ui/index.html")) ~
      getFromResourceDirectory("swagger-ui") ~
      redirectToSwagger

}
