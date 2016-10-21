package io.scalac.elm.core

import akka.actor.ActorRef
import io.scalac.elm.transaction.{ElmBlock, ElmTransaction}
import scorex.core.LocalInterface
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

class ElmLocalInterface(override val viewHolderRef: ActorRef, forgerRef: ActorRef)
  extends LocalInterface[PublicKey25519Proposition, ElmTransaction, ElmBlock] {

  override protected def onStartingPersistentModifierApplication(pmod: ElmBlock): Unit = ???

  override protected def onFailedTransaction(tx: ElmTransaction): Unit = ???

  override protected def onFailedModification(mod: ElmBlock): Unit = ???

  override protected def onSuccessfulTransaction(tx: ElmTransaction): Unit = ???

  override protected def onSuccessfulModification(mod: ElmBlock): Unit = ???
}
