package scorex.core

import scorex.core.NodeViewComponent.ComponentType

import scala.reflect.ClassTag

/** Base trait for all node view operations, which should be sent to a memory node view component
  */
trait NodeViewComponentOperation

object NodeViewComponentOperation {

  /** Get the reader for the memory pool, returns a component reader instance
    */
  case class GetReader(componentType: ComponentType) extends NodeViewComponentOperation

  /** Mode for the memory pool operation
    */
  trait OperationMode[Op <: NodeViewComponentOperation]
}
