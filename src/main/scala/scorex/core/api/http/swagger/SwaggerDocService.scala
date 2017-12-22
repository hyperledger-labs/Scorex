package scorex.core.api.http.swagger

import akka.actor.ActorSystem
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.Info
import io.swagger.models.Swagger
import scorex.core.settings.RESTApiSettings

class SwaggerDocService(system: ActorSystem, val apiClasses: Set[Class[_]], settings: RESTApiSettings)
  extends SwaggerHttpService {
  override val host: String = settings.bindAddress.toString
  override val apiDocsPath: String = "swagger"

  override val info: Info = settings.swaggerInfo

  private def prependSlashIfNecessary(path: String): String  = if(path.startsWith("/")) path else s"/$path"

  //Let swagger-ui determine the host and port
  override def swaggerConfig: Swagger = new Swagger().basePath(prependSlashIfNecessary(basePath)).info(info).scheme(scheme)
}
