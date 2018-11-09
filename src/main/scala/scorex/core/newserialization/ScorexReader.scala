package scorex.core.newserialization

import akka.util.ByteString

trait ScorexReader extends Reader {

  /**
    * Decode String is shorter than 256 bytes
    * @return
    */
  def getShortString(): String
  def getByteString2(size: Int): ByteString
  def newReader(chunk: CH): ScorexReader.Aux[CH]
}

object ScorexReader {
  type Aux[CCH] = ScorexReader{ type CH = CCH }
}
