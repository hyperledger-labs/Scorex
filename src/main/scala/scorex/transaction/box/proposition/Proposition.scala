package scorex.transaction.box.proposition

import com.google.common.primitives.Ints
import scorex.serialization.BytesSerializable


trait Proposition extends BytesSerializable


trait EmptyProposition extends Proposition

case class HeightOpenProposition(height: Int) extends Proposition {
  override lazy val bytes = Ints.toByteArray(height)
}
