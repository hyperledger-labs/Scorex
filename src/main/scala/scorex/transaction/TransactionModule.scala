package scorex.transaction

import scorex.block.TransactionalData
import scorex.settings.Settings
import scorex.transaction.box.proposition.{AddressableProposition, Proposition}
import scorex.transaction.proof.Proof
import scorex.transaction.state.{MinimalState, SecretHolder, SecretHolderGenerator}
import scorex.utils.ScorexLogging
import scorex.transaction.wallet.Wallet


trait TransactionModule[P <: Proposition, TX <: Transaction[P, TX], TBD <: TransactionalData[TX]]
  extends UnconfirmedTransactionsDatabase[TX] with MinimalState[P, TX] with ScorexLogging {

  type SH <: SecretHolder[P with AddressableProposition, _ <: Proof[P]]

  val settings: Settings

  val generator: SecretHolderGenerator[SH]
  val wallet: Wallet[P, P with AddressableProposition, _ <: TransactionModule[P, TX, TBD]]

  def isValid(blockData: TBD): Boolean

  def transactions(blockData: TBD): Seq[TX]

  def totalFee(blockData: TBD): Long = transactions(blockData).map(_.fee).sum

  def stop(): Unit = wallet.close()

  def packUnconfirmed(): TBD

  def clearFromUnconfirmed(data: TBD): Unit

  def onNewOffchainTransaction(transaction: TX): Unit

  val genesisData: TBD
}
