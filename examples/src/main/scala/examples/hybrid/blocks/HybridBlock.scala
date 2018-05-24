package examples.hybrid.blocks

import examples.commons.SimpleBoxTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block

trait HybridBlock extends PersistentNodeViewModifier with Block[SimpleBoxTransaction]
