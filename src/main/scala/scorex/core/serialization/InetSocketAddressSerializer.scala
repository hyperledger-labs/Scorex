package scorex.core.serialization

import java.net.{InetAddress, InetSocketAddress}

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}

class InetSocketAddressSerializer extends ScorexSerializer[InetSocketAddress] {

  private val PortValueLength: Int = 4

  override def write(kryo: Kryo, output: Output, address: InetSocketAddress): Unit = {
    val addressBytes = address.getAddress.getAddress
    output.writeInt(addressBytes.length + PortValueLength)
    output.writeBytes(addressBytes)
    output.writeInt(address.getPort)
  }

  override def read(kryo: Kryo, input: Input, c: Class[InetSocketAddress]): InetSocketAddress = {
    val length = input.readInt()
    val bytes = input.readBytes(length - PortValueLength)
    val port = input.readInt()

    new InetSocketAddress(InetAddress.getByAddress(bytes), port)
  }

}

