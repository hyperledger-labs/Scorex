package examples.curvepos.transaction

import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

case class PublicKey25519NoncedBox(
                                    override val proposition: PublicKey25519Proposition,
                                    override val nonce: Long,
                                    override val value: Long
                                  ) extends PublicKeyNoncedBox[PublicKey25519Proposition]

