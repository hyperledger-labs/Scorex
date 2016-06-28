package scorex

import org.scalatest.Suites
import scorex.crypto.SigningFunctionsSpecification
import scorex.network.HandshakeSpecification

class ScorexTestSuite extends Suites(
  new SigningFunctionsSpecification,
  new HandshakeSpecification
)
