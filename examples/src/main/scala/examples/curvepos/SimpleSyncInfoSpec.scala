package examples.curvepos

import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class SimpleSyncInfo(score: BlockChain.Score) extends SyncInfo {
  override def bytes: Array[Byte] = score.toByteArray
}

object SimpleSyncInfo {
  def parse(bytes: Array[Byte]): Try[SimpleSyncInfo] = Try {
    SimpleSyncInfo(BigInt(bytes))
  }
}

object SimpleSyncInfoSpec extends SyncInfoSpec[SimpleSyncInfo](SimpleSyncInfo.parse)
