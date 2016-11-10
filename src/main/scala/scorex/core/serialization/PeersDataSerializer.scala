package scorex.core.serialization

import java.net.InetSocketAddress

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import scorex.core.network.message.BasicMsgDataTypes.PeersData

class PeersDataSerializer extends ScorexSerializer[PeersData] {

  override def write(kryo: Kryo, output: Output, data: PeersData): Unit = {
    output.writeShort(data.peers.length.toShort)
    data.peers.foreach { p =>
      kryo.writeObject(output, p)
    }
  }

  override def read(kryo: Kryo, input: Input, c: Class[PeersData]): PeersData = {
    val length = input.readShort()
    val peers = (0 until length) map { i =>
      kryo.readObject(input, classOf[InetSocketAddress])
    }
    PeersData(peers)
  }


}
