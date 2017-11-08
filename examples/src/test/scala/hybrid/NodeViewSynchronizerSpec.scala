package hybrid

import examples.commons.SimpleBoxTransaction
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.testkit.properties.NodeViewSynchronizerTests

class NodeViewSynchronizerSpec extends NodeViewSynchronizerTests[PublicKey25519Proposition,
  SimpleBoxTransaction,
  HybridBlock,
  HBoxStoredState,
  HybridSyncInfo,
  HybridHistory] with HybridGenerators
