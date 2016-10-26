package scorex.core

import scorex.core.api.http.ApiRoute

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent

  def companion: NodeViewComponentCompanion
}

trait NodeViewComponentCompanion {
  def api: ApiRoute
}
