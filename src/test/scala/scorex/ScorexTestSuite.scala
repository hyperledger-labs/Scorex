package scorex

import org.scalatest.Suites
import scorex.account.AccountSpecification
import scorex.crypto.SigningFunctionsSpecification
import scorex.network.HandshakeSpecification

class ScorexTestSuite extends Suites(
  new AccountSpecification,
  new SigningFunctionsSpecification,
  new HandshakeSpecification
)
