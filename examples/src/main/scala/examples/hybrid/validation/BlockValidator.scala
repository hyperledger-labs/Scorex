package examples.hybrid.validation

import examples.hybrid.blocks.HybridPersistentNodeViewModifier

import scala.util.Try

trait BlockValidator {
  def validate(block: HybridPersistentNodeViewModifier): Try[Unit]
}
