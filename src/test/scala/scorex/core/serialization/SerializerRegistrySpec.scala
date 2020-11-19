package scorex.core.serialization

import io.circe.Encoder
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.serialization.SerializerRegistry.SerializerRecord

import scala.reflect.ClassTag
import scala.util.{Random, Try}

class SerializerRegistrySpec extends AnyFlatSpec with Matchers {

  sealed trait Creature
  sealed case class Human(name: String, country: String) extends Creature
  sealed case class Animal(family: String, legs: Int) extends Creature

  val humanEnc: Encoder[Human] = (h: Human) =>
    Map(
      "name" -> h.name.asJson,
      "country" -> h.country.asJson
    ).asJson

  val animalEnc: Encoder[Animal] = (a: Animal) =>
    Map(
      "family" -> a.family.asJson,
      "legs" -> a.legs.asJson
    ).asJson


  "`toJson`" should "be able to generate json at runtime when encoders are registered" in {

    val reg = SerializerRegistry(Seq(SerializerRecord(humanEnc), SerializerRecord(animalEnc)))

    val c: Creature = randomCreature

    val clazz = ClassTag(c.getClass).runtimeClass

    val result = reg.toJson(clazz, c)
    result.isRight shouldBe true
    result.right.get shouldBe Try(humanEnc.apply(c.asInstanceOf[Human])).getOrElse(animalEnc.apply(c.asInstanceOf[Animal]))
  }


  "`toJson`" should "generate an exception when encoders are not registered" in {

    val reg = SerializerRegistry(Seq(SerializerRecord(humanEnc)))

    val c: Creature = Animal("big cats", 4)

    val clazz = ClassTag(c.getClass).runtimeClass

    val result = reg.toJson(clazz, c)
    result.isRight shouldBe false
  }

  private def randomCreature: Creature = {
    if (Random.nextInt() % 2 == 0) Animal("big cats", 4)
    else Human("Kim", "North Korea")
  }
}
