package scorex.core.newserialization

trait ScorexWriter extends Writer {

  /**
    * Encode String is shorter than 256 bytes
    * @param s String
    * @return
    */
  def putShortString(s: String): this.type

}
