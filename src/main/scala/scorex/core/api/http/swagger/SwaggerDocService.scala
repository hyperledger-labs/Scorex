package scorex.core.api.http.swagger

import akka.actor.ActorSystem
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Contact, Info, License}
import io.swagger.models.Swagger
import scorex.core.settings.RESTApiSettings

class SwaggerDocService(system: ActorSystem, val apiClasses: Set[Class[_]], settings: RESTApiSettings)
  extends SwaggerHttpService {
  override val host: String = settings.bindAddress + ":" + settings.port
  override val apiDocsPath: String = "swagger"

  override val info: Info = Info("The Web Interface to the Scorex API",
    "1.3.0-SNAPSHOT",
    "Scorex API",
    "License: Creative Commons CC0",
    Some(Contact("Alex", "https://scorex-dev.groups.io/g/main", "alex.chepurnoy@iohk.io")),
    Some(License("License: Creative Commons CC0", "https://github.com/ScorexProject/Scorex/blob/master/COPYING"))
  )

  private def prependSlashIfNecessary(path: String): String  = if(path.startsWith("/")) path else s"/$path"

  //Let swagger-ui determine the host and port
  override def swaggerConfig: Swagger = new Swagger().basePath(prependSlashIfNecessary(basePath)).info(info).scheme(scheme)
}
