package scorex.transaction

import scorex.block.TransactionalData
import scorex.settings.Settings
import scorex.transaction.box.proposition.Proposition
import scorex.utils.ScorexLogging

//todo: make BytesParseable[TData] an instance also, not a mixin
trait TransactionalModule[P <: Proposition, TX <: Transaction[P, TX], TData <: TransactionalData[TX]]
  extends ScorexLogging {

  val settings: Settings

  def transactions(blockData: TData): Seq[TX]

  def totalFee(blockData: TData): Long = transactions(blockData).map(_.fee).sum

  def generateTdata(timeOpt: Long): TData

  val genesisData: TData
}