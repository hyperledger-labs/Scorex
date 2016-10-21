package io.scalac.elm.consensus

import scorex.core.consensus.{BlockChain, SyncInfo}
import scorex.core.network.message.SyncInfoSpec

import scala.util.Try

case class ElmSyncInfo(score: BlockChain.Score) extends SyncInfo {
  override def bytes: Array[Byte] = score.toByteArray
}

object ElmSyncInfo {
  def parse(bytes: Array[Byte]): Try[ElmSyncInfo] = Try {
    ElmSyncInfo(BigInt(bytes))
  }
}

object ElmSyncInfoSpec extends SyncInfoSpec[ElmSyncInfo](ElmSyncInfo.parse)
