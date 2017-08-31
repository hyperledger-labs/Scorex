package scorex.testkit.utils

import org.scalacheck.Shrink

trait NoShrink {
  protected implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
}
