package scorex.core.api.client

import java.net.{HttpURLConnection, URL}

import io.circe
import scorex.core.settings.Settings

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}


class ApiClient(settings: Settings) {
  private val OkHttpCode = 200

  def executeCommand(command: String): String = {
    if (command.equals("help")) {
      "<method> <url> <data> \n Type quit to stop."
    } else Try {
      val args = command.split(" ")
      val method = args.head.toUpperCase
      val path = args(1)

      val content = if (method.equals("POST")) {
        command.substring((method + " " + path + " ").length())
      } else ""

      val url = new URL("http://127.0.0.1:" + settings.rpcPort + "/" + path)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod(method)

      if (method.equals("POST")) {
        connection.setDoOutput(true)
        connection.getOutputStream.write(content.getBytes)
        connection.getOutputStream.flush()
        connection.getOutputStream.close()
      }

      val stream = connection.getResponseCode match {
        case OkHttpCode => connection.getInputStream
        case _ => connection.getErrorStream
      }

      val result = Source.fromInputStream(stream).mkString("")
      Try(result)
    }.flatten match {
      case Success(result) => result
      case Failure(e) =>
        s"Problem occurred $e! \n Type help to get a list of commands."
    }
  }
}

object ApiClient extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  val settings = new Settings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  val apiClient = new ApiClient(settings)

  println("Welcome to the Scorex command-line client...")
  Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
    println(s"[$command RESULT] " + apiClient.executeCommand(command))
  }
}
