package scorex.core.network

import scorex.core.network.message.MessageSpec
import scorex.util.ScorexLogging

import scala.util.{Failure, Success}

trait Synchronizer extends ScorexLogging {

  // these are the case statements for identifying the message handlers
  protected val msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit]

  /**
    * This method will attempt to parse a message from a remote peer into it class representation and use
    * the defined message handlers for processing the message
    *
    * @param spec the message specification (basically a header informing of the message type)
    * @param msgBytes a ByteString of the message data that must be parsed
    * @param source the remote peer that sent the message
    */
  protected def parseAndHandle(spec: MessageSpec[Any], msgBytes: Array[Byte], source: ConnectedPeer): Unit = {
    // attempt to parse the message
    spec.parseBytesTry(msgBytes) match {
      // if a message could be parsed, match the type of content found and ensure a handler is defined
      case Success(content) =>
        val parsedMsg = (spec, content, source)
        if (msgHandlers.isDefinedAt(parsedMsg)) {
          msgHandlers.apply(parsedMsg)
        } else {
          log.error(s"Function handler not found for the parsed message: $parsedMsg")
        }

      // if a message could not be parsed, penalize the remote peer
      case Failure(e) =>
        log.error(s"Failed to deserialize data from $source: ", e)
        penalizeMaliciousPeer(source)
    }
  }

  /**
    * Handles how a peer that sent un-parsable data should be handled
    *
    * @param peer peer that sent the offending message
    */
  protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit

}
