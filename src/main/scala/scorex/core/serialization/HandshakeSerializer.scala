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
  /*
    override def write(kryo: Kryo, output: Output, data: Handshake): Unit = {
      val anb = data.applicationName.getBytes

      val fab = data.declaredAddress.map { isa =>
        isa.getAddress.getAddress ++ Ints.toByteArray(isa.getPort)
      }.getOrElse(Array[Byte]())

      val nodeNameBytes = data.nodeName.getBytes

      output.writeByte(anb.size.toByte)
      output.writeBytes(anb)
      output.writeBytes(data.applicationVersion.bytes)
      output.writeByte(nodeNameBytes.size.toByte)
      output.writeBytes(nodeNameBytes)
      output.writeLong(data.nodeNonce)
      output.writeInt(fab.length)
      output.writeBytes(fab)
      output.writeLong(data.time)
    }

    override def read(kryo: Kryo, input: Input, c: Class[Handshake]): Handshake = {
      val anbSize = input.readByte()
      val an = new String(input.readBytes(anbSize))
      val av = ApplicationVersion.parseBytes(input.readBytes(ApplicationVersion.SerializedVersionLength)).get
      val nodeNameBytesLength = input.readByte()
      val nodeName = new String(input.readBytes(nodeNameBytesLength))
      val nonce = input.readLong()
      val fas = input.readInt()
      val isaOpt = if (fas > 0) {
        val fa = input.readBytes(fas - 4)
        val port = input.readInt()
        Some(new InetSocketAddress(InetAddress.getByAddress(fa), port))
      } else None

      val time = input.readLong()
      Handshake(an, av, nodeName, nonce, isaOpt, time)
    }
  */


}
