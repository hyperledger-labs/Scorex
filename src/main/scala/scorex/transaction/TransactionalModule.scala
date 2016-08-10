package scorex.transaction

import scorex.NodeStateHolder
import scorex.block.TransactionalData
import scorex.settings.Settings
import scorex.transaction.box.proposition.{AddressableProposition, Proposition}
import scorex.transaction.proof.Proof
import scorex.transaction.state.{SecretHolder, SecretHolderGenerator}
import scorex.transaction.wallet.Wallet
import scorex.utils.ScorexLogging

//todo: make BytesParseable[TData] an instance also, not a mixin
trait TransactionalModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX]]
  extends ScorexLogging {

  type SH <: SecretHolder[P with AddressableProposition, _ <: Proof[P]]
  type W <: Wallet[_ <: P, _ <: TransactionalModule[P, TX, TData]]

  val stateHolder: NodeStateHolder[P, TX, TData, _]

  val settings: Settings

  val generator: SecretHolderGenerator[SH]
  val wallet: W

  def isValid(blockData: TData): Boolean

  def transactions(blockData: TData): Seq[TX]

  def totalFee(blockData: TData): Long = transactions(blockData).map(_.fee).sum

  def generateTdata(timeOpt: Long): TData

  val genesisData: TData

  def stop(): Unit = wallet.close()
}