package scorex.transaction

import scorex.block.{Block, TransactionalData}
import scorex.settings.Settings
import scorex.transaction.box.{AddressableProposition, Proposition}
import scorex.transaction.proof.Proof
import scorex.transaction.state.{MinimalState, SecretHolder, SecretHolderGenerator}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet


trait TransactionModule[P <: Proposition, TX <: Transaction[P, TX], TBD <: TransactionalData[TX]]
  extends UnconfirmedTransactionsDatabase[TX] with MinimalState[P, TX] with ScorexLogging {

  type SH <: SecretHolder[P with AddressableProposition, _ <: Proof[P]]

  val settings: Settings

  val generator: SecretHolderGenerator[SH]
  val wallet: Wallet[P, P with AddressableProposition, _ <: TransactionModule[P, TX, TBD]]

  def isValid(block: Block[P, _, TBD]): Boolean

  def transactions(block: Block[P, _, TBD]): Seq[TX]

  def stop(): Unit = {
    wallet.close()
  }

  def packUnconfirmed(): TBD

  def clearFromUnconfirmed(data: TBD): Unit

  def onNewOffchainTransaction(transaction: TX): Unit

  val genesisData: TBD
}