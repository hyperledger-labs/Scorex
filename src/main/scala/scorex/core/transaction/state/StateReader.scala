package scorex.core.transaction.state

import scorex.core.{NodeViewComponent, VersionTag}

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int

}
