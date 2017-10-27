package hybrid

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HWallet
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.properties.NodeViewSynchronizerTests


// todo: remove unused traits
class NodeViewSynchronizerSpec extends NodeViewSynchronizerTests[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HBoxStoredState,
  HybridSyncInfo,
  HybridHistory,
  SimpleBoxTransactionMemPool,
  HWallet] with HybridGenerators
