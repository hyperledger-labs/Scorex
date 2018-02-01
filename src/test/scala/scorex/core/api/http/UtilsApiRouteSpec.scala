package scorex.core.api.http

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings
import scala.language.postfixOps

import scala.concurrent.duration._

class UtilsApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs  {

  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  val addr = new InetSocketAddress("localhost", 8080)
  val restApiSettings = RESTApiSettings(addr, None, false, None, 10 seconds)
  val prefix = "/utils"
  val routes = UtilsApiRoute(restApiSettings).route

  it should "send random seeds" in {
    Get(prefix + "/seed") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }

    Get(prefix + "/seed/32") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }

    Get(prefix + "/seed/64") ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  val msg = "hash_me"

  it should "hash string with blake2b" in {
    Post(prefix + "/hash/blake2b", msg) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
      responseAs[String] should not be msg
    }
  }

}
