package scorex.core.newserialization

import akka.util.ByteString

trait ScorexWriter extends Writer {

  /**
    * Encode String is shorter than 256 bytes
    * @param s String
    * @return
    */
  def putShortString(s: String): this.type

  def length(): Int

  def newWriter(): ScorexWriter.Aux[CH]

  def putByteString2(byteString: ByteString): this.type

  def append(scorexWriter: ScorexWriter.Aux[CH]): this.type = {
    putChunk(scorexWriter.result())
  }

  def result(): CH
}


object ScorexWriter {
  type Aux[CCH] = ScorexWriter { type CH = CCH }
}
