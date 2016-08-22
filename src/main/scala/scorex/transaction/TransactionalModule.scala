package scorex.transaction

import scorex.block.TransactionalData
import scorex.settings.Settings
import scorex.transaction.box.proposition.Proposition
import scorex.transaction.state.MinimalState
import scorex.transaction.wallet.WalletOld
import scorex.utils.ScorexLogging

//todo: make BytesParseable[TData] an instance also, not a mixin
trait TransactionalModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX]]
  extends ScorexLogging {

  //type SH <: SecretHolder[P with AddressableProposition, _ <: Proof[P]]
  type W <: WalletOld[_ <: P, _ <: TransactionalModule[P, TX, TData]]

  val mempool: MemoryPool[TX]
  val state: MinimalState[P, TX]

  val settings: Settings

  //val generator: SecretHolderGenerator[SH]
  val wallet: W

  def transactions(blockData: TData): Seq[TX]

  def totalFee(blockData: TData): Long = transactions(blockData).map(_.fee).sum

  def generateTdata(timeOpt: Long): TData

  val genesisData: TData

  def stop(): Unit = wallet.close()
}