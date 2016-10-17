package examples.curvepos

import examples.curvepos.transaction._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}

import scala.util.{Failure, Success}

class SimpleNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, SimpleTransaction, SimpleBlock] {
  override type HIS = SimpleBlockchain
  override type MS = SimpleState
  override type VL = SimpleWallet
  override type MP = SimpleMemPool

  override lazy val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = ???

  override def restoreState(): Option[(HIS, MS, VL, MP)] = None

  override protected def genesisState: (HIS, MS, VL, MP) = {
    val emptyBlockchain = new SimpleBlockchain
    val emptyState = new SimpleState
    val wallet = SimpleWallet()

    wallet.generateNewSecret()
    val acc = wallet.secrets.head
    val IntitialBasetarget = 153722867L
    val generator = PublicKey25519Proposition(Array.fill(SimpleBlock.SignatureLength)(0: Byte))
    val toInclude: Seq[SimpleTransaction] = Seq(SimplePayment(acc.publicImage, acc.publicImage, Long.MaxValue, 1, 1, 0))

    val genesisBlock: SimpleBlock = SimpleBlock(Array.fill(SimpleBlock.SignatureLength)(-1: Byte),
      0L, Array.fill(SimpleBlock.SignatureLength)(0: Byte), IntitialBasetarget, generator, toInclude)
    val blockchain = emptyBlockchain.append(genesisBlock) match {
      case Failure(f) => throw f
      case Success(newBlockchain) => newBlockchain._1
    }
    require(blockchain.height() == 1, s"${blockchain.height()} == 1")

    val state = emptyState.applyModifier(genesisBlock) match {
      case Failure(f) => throw f
      case Success(newState) => newState
    }
    require(!state.isEmpty)

    (blockchain, state, wallet, new SimpleMemPool)
  }
}
