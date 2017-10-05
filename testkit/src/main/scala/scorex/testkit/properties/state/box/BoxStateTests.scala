package scorex.testkit.properties.state.box

import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.mid.state.BoxMinimalState
import scorex.testkit.properties.state.StateTests


trait BoxStateTests[P <: Proposition,
                    B <: Box[P],
                    TX <: BoxTransaction[P, B],
                    PM <: PersistentNodeViewModifier,
                    BST <: BoxMinimalState[P, B, TX, PM, BST]] extends StateTests[PM, BST]{
}
