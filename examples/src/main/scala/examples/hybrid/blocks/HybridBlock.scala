package examples.hybrid.blocks

import examples.commons.SimpleBoxTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.block.Block
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridBlock extends PersistentNodeViewModifier with Block[PublicKey25519Proposition, SimpleBoxTransaction]
