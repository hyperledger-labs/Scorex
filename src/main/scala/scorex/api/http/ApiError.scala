package scorex.api.http

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.language.implicitConversions

case class ApiError(error: Int, message: String)

object ApiError {

  implicit def encodeToJson(err: ApiError): Json = err.asJson

  def json(t: Throwable): Json = ApiError(0, t.getMessage)

  val unknown: Json = ApiError(0, "Error is unknown")
  val wrongJson: Json = ApiError(1, "Failed to parse json message")
  val apiKeyNotValid: Json = ApiError(2, "Provided API key is not correct")
  val invalidSignature: Json = ApiError(101, "Invalid signature")
  val invalidAddress: Json = ApiError(102, "Invalid address")
  val invalidSeed: Json = ApiError(103, "Invalid seed")
  val invalidAmount: Json = ApiError(104, "Invalid amount")
  val invalidFee: Json = ApiError(105, "Invalid fee")
  val invalidSender: Json = ApiError(106, "Invalid sender")
  val invalidRecipient: Json = ApiError(107, "Invalid recipient")
  val invalidPublicKey: Json = ApiError(108, "Invalid public key")
  val invalidNotNumber: Json = ApiError(109, "Argument is not a number")
  val invalidMessage: Json = ApiError(110, "Invalid message")
  val blockNotExists: Json = ApiError(301, "Block does not exist")
}