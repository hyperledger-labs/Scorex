package scorex.core.serialization

import java.net.{InetAddress, InetSocketAddress}

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.app.ApplicationVersion
import scorex.core.network.Handshake

class HandshakeSerializer extends NullableSerializer[Handshake] {
  private val PortValueLength: Int = 4

  override def write(kryo: Kryo, output: Output, handshake: Handshake): Unit = {
    kryo.writeObject(output, handshake.applicationName, new ByteLengthUtf8StringSerializer)
    kryo.writeObject(output, handshake.applicationVersion)
    kryo.writeObject(output, handshake.nodeName, new ByteLengthUtf8StringSerializer)
    output.writeLong(handshake.nodeNonce)

    if (handshake.declaredAddress.isDefined) {
      val address = handshake.declaredAddress.get
      val addressBytes = address.getAddress.getAddress
      output.writeInt(addressBytes.length + PortValueLength)
      output.writeBytes(addressBytes)
      output.writeInt(address.getPort)
    } else output.writeInt(0)

    output.writeLong(handshake.time)
  }

  override def read(kryo: Kryo, input: Input, c: Class[Handshake]): Handshake = {
    val applicationName = kryo.readObject(input, classOf[String], new ByteLengthUtf8StringSerializer)
    val applicationVersion = kryo.readObject(input, classOf[ApplicationVersion])
    val nodeName = kryo.readObject(input, classOf[String], new ByteLengthUtf8StringSerializer())
    val nonce = input.readLong()

    val length = input.readInt()
    val declaredAddress = if (length == 0) None
    else {
      val bytes = input.readBytes(length - PortValueLength)
      val port = input.readInt()
      val address = new InetSocketAddress(InetAddress.getByAddress(bytes), port)

      Some(address)
    }

    val time = input.readLong()

    Handshake(applicationName, applicationVersion, nodeName, nonce, declaredAddress, time)
  }


}
