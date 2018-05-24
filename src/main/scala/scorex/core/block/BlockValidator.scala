package scorex.core.block

import scala.util.Try

trait BlockValidator[PM <: Block[_]] {
  def validate(block: PM): Try[Unit]
}
