package examples.tailchain.core

case class PartialProof(id: Array[Byte], rootHash: Array[Byte], proof: Array[Byte])

case class Ticket(minerKey: Array[Byte], nonce: Array[Byte])
