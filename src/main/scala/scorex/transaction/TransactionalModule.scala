package scorex.transaction

import scorex.block.TransactionalData
import scorex.serialization.BytesParsable
import scorex.settings.Settings
import scorex.transaction.box.proposition.{AddressableProposition, Proposition}
import scorex.transaction.proof.Proof
import scorex.transaction.state.{MinimalState, SecretHolder, SecretHolderGenerator}
import scorex.utils.ScorexLogging
import scorex.transaction.wallet.Wallet


trait TransactionalModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX]]
  extends UnconfirmedTransactionsDatabase[TX, TData] with MinimalState[P, TX] with ScorexLogging
  with BytesParsable[TData] {

  type SH <: SecretHolder[P with AddressableProposition, _ <: Proof[P]]
  type W <: Wallet[_ <: P, _ <: TransactionalModule[P, TX, TData]]

  val settings: Settings

  val generator: SecretHolderGenerator[SH]
  val wallet: W

  def isValid(blockData: TData): Boolean

  def transactions(blockData: TData): Seq[TX]

  def totalFee(blockData: TData): Long = transactions(blockData).map(_.fee).sum

  val genesisData: TData

  def stop(): Unit = wallet.close()
}