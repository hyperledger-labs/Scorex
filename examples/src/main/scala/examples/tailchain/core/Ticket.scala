package examples.tailchain.core

import com.google.common.primitives.Ints
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Blake2b256

import scala.util.Try

case class Ticket(minerKey: Array[Byte], nonce: Array[Byte], partialProofs: IndexedSeq[PartialProof])
  extends Serializer[Ticket] {

  override def toBytes(obj: Ticket): Array[Byte] = {
    ???
//    minerKey ++ nonce ++ partialProofs.reduce(_ ++ _)
  }


  //todo: for Dmitry: implement
  override def parseBytes(bytes: Array[Byte]): Try[Ticket] = ???
}