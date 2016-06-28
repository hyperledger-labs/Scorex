package scorex.transaction.state

import scorex.crypto.signatures.Curve25519
import scorex.serialization.BytesSerializable
import scorex.transaction.box._
import scorex.transaction.proof.{Proof, Signature25519}
import scorex.transaction.state.PrivateKey25519Holder.PrivateKey25519
import shapeless.Sized

import scala.util.Try

trait SecretHolder[P <: AddressableProposition, PR <: Proof[P]] extends BytesSerializable {
  type Secret

  val publicCommitment: P

  lazy val publicAddress = publicCommitment.address

  val secret: Secret

  def owns(box: Box[P]): Boolean

  def sign(message: Array[Byte]): PR

  def verify(message: Array[Byte], signature: PR): Boolean
}

case class PrivateKey25519Holder(override val secret: PrivateKey25519,
                                 override val publicCommitment: PublicKey25519Proposition) extends SecretHolder[PublicKey25519Proposition, Signature25519] {
  override type Secret = PrivateKey25519

  lazy val address = publicCommitment.address

  override def sign(message: Array[Byte]): Signature25519 =
    Signature25519(Curve25519.sign(secret.unsized, message))

  override def owns(box: Box[PublicKey25519Proposition]): Boolean = box.lock.publicKey.unsized sameElements publicCommitment.publicKey.unsized

  override def verify(message: Array[Byte], signature: Signature25519): Boolean =
    Curve25519.verify(signature.signature, message, publicCommitment.publicKey.unsized)

  override def bytes: Array[Byte] = secret.unsized ++ publicCommitment.publicKey.unsized
}

object PrivateKey25519Holder {
  type PrivateKey25519 = Sized[Array[Byte], SizedConstants.PrivKey25519]
}


trait SecretHolderGenerator[SH <: SecretHolder[_, _]] {
  def generateKeys(randomSeed: Array[Byte]): SH

  def parse(bytes: Array[Byte]): Try[SH]
}

object SecretGenerator25519 extends SecretHolderGenerator[PrivateKey25519Holder] {
  override def generateKeys(randomSeed: Array[Byte]): PrivateKey25519Holder = ???

  override def parse(bytes: Array[Byte]): Try[PrivateKey25519Holder] = ???
}