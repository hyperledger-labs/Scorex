package scorex.settings

import shapeless.{Nat, Succ}

object SizedConstants {
  type Nat32 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat._22]]]]]]]]]]

  type Nat40 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat32]]]]]]]]

  type Nat50 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat40]]]]]]]]]]

  type Nat60 = Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Succ[Nat50]]]]]]]]]]

  type Nat64 = Succ[Succ[Succ[Succ[Nat60]]]]

  type PrivKey25519 = Nat32

  type PubKey25519 = Nat32

  type Signature25519 = Nat64
}