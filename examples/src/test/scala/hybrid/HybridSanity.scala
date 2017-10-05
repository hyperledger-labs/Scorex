package hybrid

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.blocks.{HybridBlock, PosBlock}
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
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
  HybridHistory] with BlockchainPerformance[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HybridSyncInfo,
  SimpleBoxTransactionMemPool,
  HBoxStoredState,
  HybridHistory]
  with HybridGenerators {


  //Node view components
  override lazy val memPool: SimpleBoxTransactionMemPool = SimpleBoxTransactionMemPool.emptyPool
  override lazy val wallet = (0 until 100).foldLeft(HWallet.readOrGenerate(settings, "p"))((w, _) => w.generateNewSecret())
}