package io.scalac.elm.state

import io.scalac.elm.state.ElmWallet._
import io.scalac.elm.transaction.{ElmTransaction, TxInput, TxOutput}
import io.scalac.elm.util._
import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.Constants25519.PrivKeyLength
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.util.Try

object ElmWallet {
  def generateSecret(seed: Array[Byte] = Random.randomBytes(PrivKeyLength)): PrivateKey25519 = {
    val pair = Curve25519.createKeyPair(seed)
    PrivateKey25519(pair._1, pair._2)
  }

  case class TimedTxOutput(out: TxOutput, timestamp: Long)

  //TODO: config
  val baseFee = 100L
}


/**
  *
  * @param secret key pair
  * @param chainTxOutputs stores confirmed unspent outputs relevant to this Wallet
  * @param currentBalance sum of confirmed unspent outputs
  */
case class ElmWallet(secret: PrivateKey25519 = generateSecret(),
                     chainTxOutputs: Map[ByteKey, TimedTxOutput] = Map(),
                     currentBalance: Long = 0)
  extends Wallet[PublicKey25519Proposition, ElmTransaction, ElmWallet] {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  override type NVCT = ElmWallet

  private val pubKeyBytes: ByteKey = secret.publicKeyBytes

  override def secretByPublicImage(publicImage: PI): Option[S] = {
    if (publicImage.address == secret.publicImage.address) Some(secret) else None
  }

  /**
    * Only one secret is supported so this method always returns unmodified wallet
    */
  override def generateNewSecret(): ElmWallet = this

  override def secrets: Set[S] = Set(secret)

  override def rollback(to: VersionTag): Try[ElmWallet] = ???

  override def publicKeys: Set[PublicKey25519Proposition] = Set(secret.publicImage)

  override def scan(tx: ElmTransaction, offchain: Boolean): ElmWallet = {

    if (offchain) {
      // reduce balance by relevant inputs

      val outs = for {
        txIn <- tx.inputs
        txOut <- chainTxOutputs.get(txIn.closedBoxId)
      } yield txOut.out

      val sum = outs.map(_.value).sum
      val outIds = outs.map(_.id)

      val reducedChainTxOuts = outIds.foldLeft(chainTxOutputs)((m, k) => m - k)

      //TODO how to restore balance when transactions are forgotten?
      ElmWallet(secret, reducedChainTxOuts, currentBalance - sum)

    } else {
      // increase balance by confirmed outputs

      val outs = tx.outputs.filter(_.proposition.pubKeyBytes.key == pubKeyBytes)
      val increasedOutputs = outs.foldLeft(chainTxOutputs)((m, o) => m + (o.id.key -> TimedTxOutput(o, tx.timestamp)))
      val sum = outs.map(_.value).sum

      ElmWallet(secret, increasedOutputs, currentBalance + sum)
    }
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, ElmTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] =
    Seq()

  override def bulkScan(txs: Seq[ElmTransaction], offchain: Boolean): ElmWallet =
    txs.foldLeft(this) { case(wallet, tx) => wallet.scan(tx, offchain)}


  override def companion: NodeViewComponentCompanion = ???

  def accumulatedCoinAge: BigInt =
    chainTxOutputs.values.map(coinAge(System.currentTimeMillis)).sum

  def createPayment(to: PublicKey25519Proposition, amount: Long, priority: Int): Option[ElmTransaction] = {
    //use newest outputs to save coin-age for mining

    val currentTime = System.currentTimeMillis()
    val sortedOuts = chainTxOutputs.values.toList.sortBy(coinAge(currentTime)).reverse.map(_.out)
    val fee = priority * baseFee
    val requiredAmount = amount + fee

    val requiredOuts = findSufficientOutputs(sortedOuts, requiredAmount)
    val foundSum = requiredOuts.map(_.value).sum

    if (foundSum < requiredAmount)
      None
    else {
      val inputs = requiredOuts.map { out =>
        val signature = Signature25519(Curve25519.sign(secret.privKeyBytes, out.bytes))
        TxInput(out.id, signature)
      }
      val change = TxOutput(foundSum - requiredAmount, secret.publicImage)
      val toRecipient = TxOutput(amount, secret.publicImage)
      val outputs = if (change.value > 0) List(toRecipient, change) else List(toRecipient)
      Some(ElmTransaction(inputs, outputs, fee, currentTime))
    }
  }

  private def findSufficientOutputs(outs: List[TxOutput], value: Long): List[TxOutput] =
    if (value > 0) {
      outs match {
        case hd :: tl => hd :: findSufficientOutputs(tl, value - hd.value)
        case Nil => Nil
      }
    } else Nil

  private def coinAge(currentTime: Long)(out: TimedTxOutput): BigInt =
    BigInt(currentTime - out.timestamp) * out.out.value
}
