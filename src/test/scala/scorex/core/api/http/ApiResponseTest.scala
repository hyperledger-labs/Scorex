package scorex.core.api.http

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.Json
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class ApiResponseTest extends AnyFlatSpec with Matchers with ScalatestRouteTest {

  private val request = HttpRequest()

  "ApiResponse" should "support json objects" in {
    val route = ApiResponse(Map("msg" -> "OK").asJson)
    request ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should include("\"OK\"")
    }
  }

  it should "wrap objects to json" in {
    val route = ApiResponse(Map("msg" -> "OK"))
    request ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should include("\"OK\"")
    }
  }

  it should "return 404 for Null" in {
    val route = ApiResponse(None.asJson)
    request ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return 404 for None" in {
    val route = ApiResponse(None)
    request ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "support future json" in {
    val route = ApiResponse(Future.successful(Map("msg" -> "OK").asJson))
    request ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should include("\"OK\"")
    }
  }

  it should "wrap future objects to json" in {
    val route = ApiResponse(Future.successful(Map("msg" -> "OK")))
    request ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should include("\"OK\"")
    }
  }

  it should "return 404 for Null future json" in {
    val route = ApiResponse(Future.successful(None.asJson))
    request ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return 404 for future None" in {
    val route = ApiResponse(Future.successful(None))
    request ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return 500 for failure" in {
    import ApiErrorHandler.exceptionHandler
    val route = ApiResponse(Future.failed[Json](new Exception))
    request ~> route ~> check {
      status shouldBe StatusCodes.InternalServerError
    }
  }

  it should "wrap strings to json" in {
    val route = ApiResponse.OK.withString("str")
    request ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldBe "\"str\""
    }
  }

}
