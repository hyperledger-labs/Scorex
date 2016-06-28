package scorex.transaction.box.sigmacoin

import scorex.transaction.box.proposition.AddressableProposition

//todo: a_reserve
//todo: sigma protocol id
//todo move to sigmacoin
sealed trait SigmaProposition extends AddressableProposition {
  val a: Array[Byte]
  val bytes = a
}
