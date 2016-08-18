package scorex.transaction.box.proposition

import scorex.transaction.state.Secret

trait PublicImage[S <: Secret] extends Proposition {
  def id: Array[Byte]

  def address: String
}
