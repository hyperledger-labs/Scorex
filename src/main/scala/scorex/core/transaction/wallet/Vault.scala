package scorex.core.transaction.wallet

import scorex.core.transaction.Transaction
import scorex.core.{PersistentNodeViewModifier, VersionTag}

import scala.util.Try

/**
  * Abstract interface for Vault, a storage for node-specific information
  */

trait Vault[TX <: Transaction, PMOD <: PersistentNodeViewModifier, V <: Vault[TX, PMOD, V]] extends VaultReader {
  self: V =>

  def scanOffchain(tx: TX): V

  def scanOffchain(txs: Seq[TX]): V

  def scanPersistent(modifier: PMOD): V

  def scanPersistent(modifiers: Option[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
    v.scanPersistent(mod)
  }

  def rollback(to: VersionTag): Try[V]

  /**
    * @return read-only copy of this state
    */
  def getReader: VaultReader = this

}