package io.scalac.elm.transaction

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Proof

case class TxInput(closedBoxId: Array[Byte], boxKey: Proof[PublicKey25519Proposition])
  extends BoxUnlocker[PublicKey25519Proposition]
