package scorex.core.app

import scorex.core.newserialization._

object Version {
  def apply(v: String): Version = {
    val splitted = v.split("\\.")
    Version(splitted(0).toByte, splitted(1).toByte, splitted(2).toByte)
  }
}
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte)

object ApplicationVersionSerializer extends ScorexSerializer[Version] {
  val SerializedVersionLength: Int = 3


  override def serialize(obj: Version, w: ScorexWriter): Unit = {
    w.put(obj.firstDigit)
    w.put(obj.secondDigit)
    w.put(obj.thirdDigit)
  }

  override def parse(r: ScorexReader): Version = {
    Version(
      r.getByte(),
      r.getByte(),
      r.getByte()
    )
  }
}