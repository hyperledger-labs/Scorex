package examples.hybrid.mining


trait MiningConstants {
  def targetBlockDelay: Long
  def RParamX10: Long

  lazy val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  lazy val Difficulty = BigInt("50")

  lazy val GenesisParentId = Array.fill(32)(1: Byte)
}
