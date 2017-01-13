package examples.hybrid.blocks

import examples.hybrid.state.SimpleBoxTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridBlock extends PersistentNodeViewModifier[PublicKey25519Proposition, SimpleBoxTransaction]
  with Block[PublicKey25519Proposition, SimpleBoxTransaction]
