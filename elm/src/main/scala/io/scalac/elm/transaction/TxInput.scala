package io.scalac.elm.transaction

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519

case class TxInput(closedBoxId: Array[Byte], boxKey: Signature25519)
  extends BoxUnlocker[PublicKey25519Proposition]
