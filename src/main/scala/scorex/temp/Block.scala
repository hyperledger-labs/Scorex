package scorex.temp


/*

 todo: move to examples

object Block extends ScorexLogging {

  def parseBytes[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  (bytes: Array[Byte])
  (implicit consensusParser: BytesParseable[CData],
   transactionalParser: BytesParseable[TData]): Try[Block[P, TData, CData]] = Try {
    val version = bytes.head
    val timestamp = Longs.fromByteArray(bytes.slice(1, 9))
    val cDataSize = Ints.fromByteArray(bytes.slice(9, 13))
    val cData = consensusParser.parseBytes(bytes.slice(13, 13 + cDataSize)).get
    val tDataSize = Ints.fromByteArray(bytes.slice(13 + cDataSize, 17 + cDataSize))
    val tData = transactionalParser.parseBytes(bytes.slice(17 + cDataSize, 17 + cDataSize + tDataSize)).get
    require(version == (cData.version + tData.version).toByte)
    new Block[P, TData, CData](timestamp, cData, tData)
  }

  def build[P <: Proposition, TX <: Transaction[P, TX], CData <: ConsensusData, TData <: TransactionalData[TX]]
  (consensusData: CData)
  (transactionalData: TData): Block[P, TData, CData] = {

    val timestamp = System.currentTimeMillis()
    new Block(timestamp, consensusData, transactionalData)
  }

  def genesis[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  (genesisTimestamp: Long)
  (implicit consensusModule: ConsensusModule[P, CData],
   transactionalModule: TransactionalModule[P, TX, TData]): Block[P, TData, CData] = {

    new Block(genesisTimestamp, consensusModule.genesisData, transactionalModule.genesisData)
  }

  /*
  def isValid[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX], CData <: ConsensusData]
  (block: Block[P, TData, CData])
  (implicit consensusModule: ConsensusModule[P, TX, TData, CData],
   transactionalModule: TransactionalModule[P, TX, TData]): Boolean = {

    if (consensusModule.contains(block)) true //applied blocks are valid
    else {
      lazy val consensus = consensusModule.isValid(block)
      lazy val transaction = transactionalModule.isValid(block.transactionalData)

      if (!consensus) log.debug(s"Invalid consensus data in block ${consensusModule.encodedId(block)}")
      else if (!transaction) log.debug(s"Invalid transaction data in block ${consensusModule.encodedId(block)}")

      consensus && transaction
    }
  }*/
}
*/