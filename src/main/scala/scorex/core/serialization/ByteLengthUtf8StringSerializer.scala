package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}

class ByteLengthUtf8StringSerializer extends NullableSerializer[String] {

  private val DefaultCharset: String = "UTF-8"

  override def write(kryo: Kryo, output: Output, string: String): Unit = {
    val bytes = string.getBytes(DefaultCharset)
    output.writeByte(bytes.length)
    output.writeBytes(bytes)
  }

  override def read(kryo: Kryo, input: Input, c: Class[String]): String = {
    val length = 0xff & input.readByte()
    val bytes = input.readBytes(length)

    new String(bytes, DefaultCharset)
  }

}
