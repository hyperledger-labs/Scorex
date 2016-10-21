package io.scalac.elm.transaction

import io.scalac.elm.util._
import scorex.core.NodeViewComponentCompanion
import scorex.core.transaction.box.proposition.Constants25519.PrivKeyLength
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
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
}

/**
  *
  * @param secret key pair
  * @param chainTxs stores confirmed transactions relevant to this Wallet
  * @param currentBalance sum of confirmed unspent outputs relevant to this Wallet
  */
case class ElmWallet(secret: PrivateKey25519 = ElmWallet.generateSecret(),
                     chainTxs: Map[ByteKey, ElmTransaction] = Map(),
                     chainTxOutputs: Map[ByteKey, TxOutput] = Map(),
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

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def scan(tx: ElmTransaction, offchain: Boolean): ElmWallet = {

    if (offchain) {
      // reduce balance by relevant inputs

      val outs = for {
        t <- chainTxs.get(tx.id)
        txIn <- t.inputs
        txOut <- chainTxOutputs.get(txIn.closedBoxId)
      } yield txOut

      val sum = outs.map(_.value).sum
      val outIds = outs.map(_.id)

      val reducedChainTxOuts = outIds.foldLeft(chainTxOutputs)((m, k) => m - k)

      //TODO how to restore balance when transactions are forgotten?
      ElmWallet(secret, chainTxs, reducedChainTxOuts, currentBalance - sum)

    } else {
      // increase balance by confirmed outputs

      val outs = tx.outputs.filter(_.proposition.pubKeyBytes.key == pubKeyBytes)
      val increasedOutputs = outs.foldLeft(chainTxOutputs)((m, o) => m + (o.id.key -> o))
      val sum = outs.map(_.value).sum

      ElmWallet(secret, chainTxs + (tx.id.key -> tx), increasedOutputs, currentBalance + sum)
    }
  }

  override def historyTransactions: Seq[WalletTransaction[PublicKey25519Proposition, ElmTransaction]] = ???

  override def historyBoxes: Seq[WalletBox[PublicKey25519Proposition]] = {
    Seq()
  }

  override def bulkScan(txs: Seq[ElmTransaction], offchain: Boolean): ElmWallet =
    txs.foldLeft(this) { case(wallet, tx) => wallet.scan(tx, offchain)}


  override def companion: NodeViewComponentCompanion = ???
}
