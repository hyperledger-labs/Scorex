package scorex.core.network.message

import scorex.core.serialization.ScorexKryoPool

import scala.reflect._
import scala.util.Try

trait KryoMessageSpec[Content] extends MessageSpec[Content] {
  protected val serializer: ScorexKryoPool
  protected val c: Class[Content]

  def deserializeData(bytes: Array[Byte]): Try[Content] = serializer.fromBytes[Content](bytes, c)

  def serializeData(data: Content): Array[Byte] = serializer.toBytesWithoutClass(data)

}
