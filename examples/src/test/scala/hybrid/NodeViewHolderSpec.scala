package hybrid

import examples.commons.{SimpleBoxTransaction, SimpleBoxTransactionMemPool}
import examples.hybrid.blocks.HybridBlock
import examples.hybrid.history.{HybridHistory, HybridSyncInfo}
import examples.hybrid.state.HBoxStoredState
import examples.hybrid.wallet.HBoxWallet
import scorex.testkit.properties.NodeViewHolderTests

class NodeViewHolderSpec extends NodeViewHolderTests[SimpleBoxTransaction, HybridBlock, HBoxStoredState,
                                                      HybridSyncInfo, HybridHistory, SimpleBoxTransactionMemPool]
  with HybridGenerators {
  type VL = HBoxWallet
}
