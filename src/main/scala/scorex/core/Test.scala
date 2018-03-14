package scorex.core

import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.SerializerRegistry
import scorex.core.serialization.SerializerRegistry.SerializerRecord

import scala.reflect.ClassTag

trait Creature

case class Human(name: String, language: String) extends Creature

case class Animal(kind: String, legs: Int) extends Creature

object Test extends App {

  val humanEnc: Encoder[Human] = (h: Human) =>
    Map(
      "name" -> h.name.asJson,
      "language" -> h.language.asJson
    ).asJson

  val animalEnc: Encoder[Animal] = (a: Animal) =>
    Map(
      "kind" -> a.kind.asJson,
      "legs" -> a.legs.asJson
    ).asJson


  val reg = SerializerRegistry(Seq(SerializerRecord(humanEnc), SerializerRecord(animalEnc)))

  val c: Creature = Animal("big cats", 4)

  val clazz = ClassTag(c.getClass).runtimeClass

  println(reg.toJson(clazz, c))
}
