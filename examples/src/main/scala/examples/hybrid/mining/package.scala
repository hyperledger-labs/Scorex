package examples.hybrid

import net.ceedubs.ficus.readers.NameMapper
import net.ceedubs.ficus.readers.namemappers.HyphenNameMapper

package object mining {
  implicit val hyphenCase: NameMapper = HyphenNameMapper
}
