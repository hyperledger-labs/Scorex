package scorex.core.transaction.wallet

import scorex.core.NodeViewComponent
import scorex.core.transaction.{NodeViewModifier, Transaction}
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

/**
  Abstract interface for Vault, a storage for node-specific information
  */

trait Vault[P <: Proposition, TX <: Transaction[P], V <: Vault[P, TX, V]] extends NodeViewComponent {
  type VersionTag = NodeViewModifier.ModifierId

  def scan(tx: TX, offchain: Boolean): V

  def bulkScan(txs: Seq[TX], offchain: Boolean): V

  def rollback(to: VersionTag): Try[V]
}