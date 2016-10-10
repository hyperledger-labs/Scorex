package scorex.core.serialization

import io.circe.Json

trait JsonSerializable {
  def json: Json
}
