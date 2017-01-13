package examples.hybrid.mining


trait MiningConstants {
  def BlockDelay: Long

  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("50")

  lazy val GenesisParentId = Array.fill(32)(1: Byte)

  lazy val MaxBlockSize = 100000

}
