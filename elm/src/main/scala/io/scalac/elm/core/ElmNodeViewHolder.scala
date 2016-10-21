package io.scalac.elm.core

import io.scalac.elm.consensus.{ElmBlockchain, ElmSyncInfo}
import io.scalac.elm.transaction._
import scorex.core.NodeViewModifier.ModifierTypeId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{NodeViewHolder, NodeViewModifier, NodeViewModifierCompanion}

import scala.util.{Failure, Success}

class ElmNodeViewHolder extends NodeViewHolder[PublicKey25519Proposition, ElmTransaction, ElmBlock] {
  override type SI = ElmSyncInfo

  override type HIS = ElmBlockchain
  override type MS = ElmMinState
  override type VL = ElmWallet
  override type MP = ElmMemPool

  override lazy val modifierCompanions: Map[ModifierTypeId, NodeViewModifierCompanion[_ <: NodeViewModifier]] = ???

  override def restoreState(): Option[(HIS, MS, VL, MP)] = None

  override protected def genesisState: (HIS, MS, VL, MP) = {
    val emptyBlockchain = new ElmBlockchain
    val emptyState = new ElmMinState
    val wallet = ElmWallet()

    wallet.generateNewSecret()
    val acc = wallet.secrets.head
    val IntitialBasetarget = 153722867L
    val generator = PublicKey25519Proposition(Array.fill(ElmBlock.SignatureLength)(0: Byte))
    val toInclude: Seq[ElmTransaction] = null //Seq(ElmPayment(acc.publicImage, acc.publicImage, Long.MaxValue, 1, 1, 0))

    val genesisBlock: ElmBlock = ElmBlock(Array.fill(ElmBlock.SignatureLength)(-1: Byte),
      0L, Array.fill(ElmBlock.SignatureLength)(0: Byte), IntitialBasetarget, generator, toInclude)
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

    log.info(s"Genesis state with block ${genesisBlock.json.noSpaces} created")

    (blockchain, state, wallet, new ElmMemPool)
  }
}
