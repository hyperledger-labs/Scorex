package scorex.core.serialization

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.app.ApplicationVersion

class ApplicationVersionSerializer extends NullableSerializer[ApplicationVersion] {
  override def write(kryo: Kryo, output: Output, version: ApplicationVersion): Unit = {
    output.writeInt(version.firstDigit)
    output.writeInt(version.secondDigit)
    output.writeInt(version.thirdDigit)
  }

  override def read(kryo: Kryo, input: Input, c: Class[ApplicationVersion]): ApplicationVersion = {
    val major = input.readInt()
    val minor = input.readInt()
    val patch = input.readInt()

    ApplicationVersion(major, minor, patch)
  }
}
