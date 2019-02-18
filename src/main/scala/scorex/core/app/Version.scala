package scorex.core.app

import scorex.core.serialization.{BytesSerializable, ScorexSerializer}
import scorex.util.serialization._

/**
  * Version of p2p protocol. Node can only process messages of it's version or lower.
  */
case class Version(firstDigit: Byte, secondDigit: Byte, thirdDigit: Byte) extends BytesSerializable with Ordered[Version] {
  override type M = Version

  override def serializer: ScorexSerializer[Version] = ApplicationVersionSerializer

  override def compare(that: Version): Int = if (this.firstDigit != that.firstDigit) {
    this.firstDigit - that.firstDigit
  } else if (this.secondDigit != that.secondDigit) {
    this.secondDigit - that.secondDigit
  } else {
    this.thirdDigit - that.thirdDigit
  }
}

object Version {
  def apply(v: String): Version = {
    val splitted = v.split("\\.")
    Version(splitted(0).toByte, splitted(1).toByte, splitted(2).toByte)
  }

  val initial: Version = Version(0, 0, 1)
  val last: Version = Version(0, 0, 1)

}

object ApplicationVersionSerializer extends ScorexSerializer[Version] {
  val SerializedVersionLength: Int = 3


  override def serialize(obj: Version, w: Writer): Unit = {
    w.put(obj.firstDigit)
    w.put(obj.secondDigit)
    w.put(obj.thirdDigit)
  }

  override def parse(r: Reader): Version = {
    Version(
      r.getByte(),
      r.getByte(),
      r.getByte()
    )
  }
}