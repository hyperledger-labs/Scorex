package scorex.core.transaction.box.proposition

import scorex.core.transaction.state.Secret

trait PublicImage[S <: Secret] extends Proposition {
  def id: Array[Byte]

  def address: String
}
