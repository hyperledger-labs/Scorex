package examples.hybrid.mining

import scala.concurrent.duration._


trait MiningConstants {
  lazy val BlockDelay = 8.seconds.toMillis

  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("50")

  lazy val GenesisParentId = Array.fill(32)(1: Byte)

  lazy val MaxBlockSize = 100000

}
