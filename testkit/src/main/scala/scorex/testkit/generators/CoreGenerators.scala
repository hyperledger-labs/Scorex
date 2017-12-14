package scorex.testkit.generators

import org.scalacheck.Gen
import scorex.ObjectGenerators
import scorex.core.VersionTag

//Generators of objects from scorex-core
trait CoreGenerators extends ObjectGenerators {
  lazy val versionTagGen: Gen[VersionTag] = modifierIdGen.map(id => VersionTag @@ id)
}