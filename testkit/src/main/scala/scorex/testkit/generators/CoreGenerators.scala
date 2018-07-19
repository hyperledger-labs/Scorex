package scorex.testkit.generators

import org.scalacheck.Gen
import scorex.ObjectGenerators
import scorex.core.{VersionTag, idToVersion}

//Generators of objects from scorex-core
trait CoreGenerators extends ObjectGenerators {
  lazy val versionTagGen: Gen[VersionTag] = modifierIdGen.map(id => idToVersion(id))
}