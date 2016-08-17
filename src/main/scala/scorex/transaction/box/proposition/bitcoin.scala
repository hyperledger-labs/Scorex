package scorex.transaction.box.proposition

import scorex.transaction.proof.Proof
import scala.util.{Failure, Success, Try}


//Bitcoin

case class StackElement(bytes: Array[Byte]) {
  def isZero: Boolean = bytes.forall(_ == StackElement.Zero)
}

object StackElement {
  val Zero = 0: Byte
}

case class State(stack: List[StackElement]) {
  def push(element: StackElement): State = State(element :: stack)

  def pop(): Try[(State, Array[Byte])] = stack match {
    case head :: tail => Success(State(tail) -> head.bytes)
    case Nil => Failure(new Exception("Empty stack"))
  }

  def isOk: Boolean = stack.head.isZero
}

object State {
  def empty: State = new State(List())
}


trait PublicImage[S <: Secret] {
  val bytes: Array[Byte]
}

case class PubKey(override val bytes: Array[Byte]) extends PublicImage[PrivateKey]

case class PubKeyHash(override val bytes: Array[Byte]) extends PublicImage[PrivateKey]

case class Hash(override val bytes: Array[Byte]) extends PublicImage[HashPreimage]


trait Secret

trait PrivateKey extends Secret

trait HashPreimage extends Secret


sealed trait BitcoinProposition extends Proposition {
  def run(state: State): Try[State]

  override def bytes: Array[Byte] = ???
}

trait Instruction extends BitcoinProposition

case class Push(byte: Byte) extends Instruction {
  override def run(state: State): Try[State] =
    Success(state.push(StackElement(Array(byte))))
}

case object Add extends Instruction {
  override def run(state: State): Try[State] =
    state.pop().flatMap { case (s1, first) =>
      s1.pop().map { case (s2, second) =>
        val sumba = (BigInt(1, first) + BigInt(1, second)).toByteArray
        state.push(StackElement(sumba))
      }
    }
}

case class AndThen(first: BitcoinProposition, second: BitcoinProposition) extends BitcoinProposition {
  def run(state: State): Try[State] = first.run(state).flatMap(s => second.run(s))
}

trait RequireProofOfKnowledge extends BitcoinProposition

case class RequireHashPreimageOfRequire(hash: Hash) extends RequireProofOfKnowledge {
  override def run(state: State): Try[State] =
    Success(state.push(StackElement(hash.bytes)))
}

case class RequirePrivateKeyOfRequire(pubKey: PubKey) extends RequireProofOfKnowledge {
  override def run(state: State): Try[State] = ???
}

case class RequirePrivateKeyOfHashedRequire(pubKeyHash: PubKeyHash) extends RequireProofOfKnowledge {
  override def run(state: State): Try[State] = ???
}


trait BitcoinProof[S <: Secret] extends Proof[BitcoinProposition] with BitcoinProposition

case class Signature(signature: Array[Byte]) extends BitcoinProof[PrivateKey] {
  override def bytes: Array[Byte] = ???

  override def isValid(proposition: BitcoinProposition, message: Array[Byte]): Boolean = ???

  override def run(state: State): Try[State] = ???
}

case class HashPreimageReveal(hashPreimage: HashPreimage) extends BitcoinProof[HashPreimage] {
  override def bytes: Array[Byte] = ???

  override def isValid(proposition: BitcoinProposition, message: Array[Byte]): Boolean = ???

  override def run(state: State): Try[State] = ???
}


trait Evaluator {
  private val InitState: State = State.empty

  def evaluate(proposition: BitcoinProposition): Boolean =
    step(proposition).map(_.isOk).getOrElse(false)

  def step(proposition: BitcoinProposition): Try[State] =
    proposition.run(InitState)
}