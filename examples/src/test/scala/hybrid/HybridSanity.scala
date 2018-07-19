package hybrid

import examples.commons.{PublicKey25519NoncedBox, SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.blocks.{HybridBlock, PosBlock}
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HBoxWallet
import org.scalacheck.Gen
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.{BlockchainPerformance, BlockchainSanity}


class HybridSanity extends BlockchainSanity[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  PosBlock,
  HybridSyncInfo,
  PublicKey25519NoncedBox,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory] with BlockchainPerformance[SimpleBoxTransaction, HybridBlock, HybridSyncInfo,
                                            SimpleBoxTransactionMemPool, HBoxStoredState, HybridHistory]
  with HybridGenerators {

  private val walletSettings = originalSettings.walletSettings.copy(seed = "p")

  //Node view components
  override lazy val memPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool.emptyPool
  override lazy val memPoolGenerator: Gen[SimpleBoxTransactionMemPool] = emptyMemPoolGen
  override lazy val transactionGenerator: Gen[TX] = simpleBoxTransactionGen
  override lazy val wallet = (0 until 100).foldLeft(HBoxWallet.readOrGenerate(walletSettings))((w, _) => w.generateNewSecret())
}