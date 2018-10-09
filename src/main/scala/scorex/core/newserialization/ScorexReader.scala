package scorex.core.newserialization

trait ScorexReader extends Reader {

  /**
    * Decode String is shorter than 256 bytes
    * @return
    */
  def getShortString(): String

}
