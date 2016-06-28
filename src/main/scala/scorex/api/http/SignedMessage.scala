package scorex.api.http

case class SignedMessage(message: String, signature: String, publickey: String)