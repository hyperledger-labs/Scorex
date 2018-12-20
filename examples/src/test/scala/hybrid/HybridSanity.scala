package hybrid

import akka.actor.{ActorRef, ActorSystem}
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
                                            HBoxStoredState, HybridHistory]
  with HybridGenerators {

  private val walletSettings = originalSettings.walletSettings.copy(seed = "p")

  override protected def createMempoolActor(system: ActorSystem): ActorRef = {
    SimpleBoxTransactionMemPool.createMempoolActor(system)
  }

  override lazy val transactionGenerator: Gen[TX] = simpleBoxTransactionGen
  override lazy val wallet = (0 until 100).foldLeft(HBoxWallet.readOrGenerate(walletSettings))((w, _) => w.generateNewSecret())
}
