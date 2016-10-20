package scorex

import org.scalatest.Suites
import scorex.crypto.SigningFunctionsSpecification
import scorex.network.MessageSpecification

class ScorexTestSuite extends Suites(
  new MessageSpecification,
  new SigningFunctionsSpecification
)
