package hybrid

import examples.curvepos.transaction.{SimpleBlock, SimplePayment, SimpleTransaction, SimpleWallet}
import examples.hybrid.history.HybridSyncInfo
import org.scalacheck.{Arbitrary, Gen}
import scorex.ObjectGenerators
import scorex.core.NodeViewModifier
import scorex.core.block.Block
import scorex.core.block.Block._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

trait HybridGenerators extends ObjectGenerators {
  val hybridSyncInfoGen: Gen[HybridSyncInfo] =  for {
    answer <- Arbitrary.arbitrary[Boolean]
    pos <- genBoundedBytes(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize)
    pow <- genBoundedBytes(NodeViewModifier.ModifierIdSize, NodeViewModifier.ModifierIdSize)
  } yield  HybridSyncInfo(answer, pow, pos)

}
