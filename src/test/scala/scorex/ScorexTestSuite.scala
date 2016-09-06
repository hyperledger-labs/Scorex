package scorex.core

import org.scalatest.Suites
import scorex.core.network.HandshakeSpecification
import scorex.crypto.SigningFunctionsSpecification

class ScorexTestSuite extends Suites(
  new SigningFunctionsSpecification,
  new HandshakeSpecification
)
