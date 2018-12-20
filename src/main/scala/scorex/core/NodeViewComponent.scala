package scorex.core

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}

object NodeViewComponent {
  trait ComponentType
  object StateComponent extends ComponentType
  object HistoryComponent extends ComponentType
  object MempoolComponent extends ComponentType
  object VaultComponent extends ComponentType
}
