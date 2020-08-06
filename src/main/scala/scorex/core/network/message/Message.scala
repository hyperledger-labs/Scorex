package scorex.core.network.message


import akka.actor.DeadLetterSuppression
import scorex.core.network.ConnectedPeer
import scala.util.{Success, Try}


/**
  * Wrapper for a network message, whether come from external peer or generated locally
  *
  * @param spec   - message specification
  * @param input  - message being wrapped, whether in byte-array form (if from outside),
  *               or structured data (if formed locally)
  * @param source - source peer, if the message is from outside
  * @tparam Content - message data type
  */
case class Message[Content](spec: MessageSpec[Content],
                            input: Either[Array[Byte], Content],
                            source: Option[ConnectedPeer])
  extends DeadLetterSuppression {

  import Message._

  /**
    * Message data bytes
    */
  lazy val dataBytes: Array[Byte] = input match {
    case Left(db) => db
    case Right(d) => spec.toBytes(d)
  }

  /**
    * Structured message content
    */
  lazy val data: Try[Content] = input match {
    case Left(db) => spec.parseBytesTry(db)
    case Right(d) => Success(d)
  }

  lazy val dataLength: Int = dataBytes.length

  /**
    * @return serialized message length in bytes
    */
  def messageLength: Int = {
    if (dataLength > 0) {
      HeaderLength + ChecksumLength + dataLength
    } else {
      HeaderLength
    }
  }

}

object Message {
  type MessageCode = Byte

  val MagicLength: Int = 4

  val ChecksumLength: Int = 4

  val HeaderLength: Int = MagicLength + 5
}
