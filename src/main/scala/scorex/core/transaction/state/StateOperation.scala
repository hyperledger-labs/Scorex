package scorex.core.transaction.state

import scorex.core.NodeViewComponent.ComponentType
import scorex.core.NodeViewComponentOperation.OperationMode
import scorex.core.consensus.History.ProgressInfo
import scorex.core.transaction.MempoolOperation.{Put, PutNormal}
import scorex.core.transaction.Transaction
import scorex.core.{NodeViewComponentOperation, PersistentNodeViewModifier}

import scala.util.Try

trait StateOperation extends NodeViewComponentOperation

object StateOperation {

  /** Update state, actor responds with `UpdateStateResponse`
    */
  case class ApplyModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD, mode: OperationMode[ApplyModifier[_]])
    extends StateOperation

  /** Result of `UpdateState` operation
    */
  case class PersistentModifierResponse[PMOD <: PersistentNodeViewModifier](updatedComponents: Set[ComponentType],
                                                                            progressInfo: ProgressInfo[PMOD],
                                                                            blocksApplied: Seq[PMOD],
                                                                            pmod: PMOD,
                                                                            mode: OperationMode[ApplyModifier[_]])

  /** Validation request, actor responds with `ValidationResponse`
    *
    * @param transaction transaction to validate
    * @param putMode     optional info to pass back to sender together with the validation result.
    *                    Is not checked during validation
    */
  case class ValidateTransaction[TX <: Transaction](transaction: TX, putMode: OperationMode[Put[_]] = PutNormal)

  /** Result of `ValidateTransaction` operation
    */
  case class TransactionValidationResponse[TX <: Transaction](transaction: TX,
                                                              validationResult: Try[Unit],
                                                              putMode: OperationMode[Put[_]])

  object LocallyGenerated extends OperationMode[ApplyModifier[_]]
  object RemotelyGenerated extends OperationMode[ApplyModifier[_]]

}
