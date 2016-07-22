package scorex.transaction.state

import scorex.crypto.signatures.Curve25519
import scorex.serialization.{BytesParsable, BytesSerializable}
import scorex.settings.SizedConstants
import scorex.transaction.box._
import scorex.transaction.box.proposition.{AddressableProposition, PublicKey25519Proposition}
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

case class PrivateKey25519Holder(secret: PrivateKey25519, publicCommitment: PublicKey25519Proposition)
  extends SecretHolder[PublicKey25519Proposition, Signature25519] with BytesSerializable {
  override type Secret = PrivateKey25519

  lazy val address = publicCommitment.address

  override def sign(message: Array[Byte]): Signature25519 =
    Signature25519(Curve25519.sign(secret.unsized, message))

  override def owns(box: Box[PublicKey25519Proposition]): Boolean = box.proposition.publicKey.unsized sameElements publicCommitment.publicKey.unsized

  override def verify(message: Array[Byte], signature: Signature25519): Boolean =
    Curve25519.verify(signature.signature, message, publicCommitment.publicKey.unsized)

  override lazy val bytes: Array[Byte] = secret.unsized ++ publicCommitment.publicKey.unsized
}

object PrivateKey25519Holder {
  type PrivateKey25519 = Sized[Array[Byte], SizedConstants.PrivKey25519]
}


trait SecretHolderGenerator[SH <: SecretHolder[_, _]] extends BytesParsable[SH] {
  def generateKeys(randomSeed: Array[Byte]): SH

}

object SecretGenerator25519 extends SecretHolderGenerator[PrivateKey25519Holder] {
  override def generateKeys(randomSeed: Array[Byte]): PrivateKey25519Holder = {
    val pair = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = Sized.wrap(pair._1)
    val pubk: PublicKey25519Proposition = PublicKey25519Proposition(Sized.wrap(pair._2))
    PrivateKey25519Holder(secret, pubk)
  }

  override def parseBytes(bytes: Array[Byte]): Try[PrivateKey25519Holder] = Try {
    PrivateKey25519Holder(Sized.wrap(bytes.slice(0, 32)), PublicKey25519Proposition(Sized.wrap(bytes.slice(32, 64))))
  }
}
