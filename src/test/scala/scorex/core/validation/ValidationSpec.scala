package scorex.core.validation

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.bytesToId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.{ScorexEncoder, ScorexEncoding}
import scorex.core.validation.ValidationResult._

import scala.util.{Failure, Try}

class ValidationSpec extends FlatSpec with Matchers with ScorexEncoding {

  val errMsg1 = "Error message 1: "
  val errMsg3 = "Error message 3: "

  val map: Map[Short, (String => Invalid, Boolean)] = Map(
    1.toShort -> (s => fatal(errMsg1 + s), true),
    3.toShort -> (s => fatal(errMsg3 + s), true),
    4.toShort -> (s => fatal(s"Error message 4: $s"), true),
    5.toShort -> (s => error(s"Error message 5: $s"), true),
    10.toShort -> (_ => error("Deactivated check"), false)
  )

  val ffSettings: TaggedValidationRules = constructValidationSettings(failFast = true, map)
  val aeSettings: TaggedValidationRules = constructValidationSettings(failFast = false, map)

  /** Start validation in Fail-Fast mode */
  def failFast: ValidationState[Unit] = ModifierValidator.failFast

  /** Start validation accumulating all the errors */
  def accumulateErrors: ValidationState[Unit] = ModifierValidator.accumulateErrors

  /** Start tagged validation in Fail-Fast mode */
  def failFastTagged: TaggedValidationState[Unit] = ModifierValidator.failFastTagged(ffSettings)

  /** Start tagged validation accumulating all the errors */
  def accumulateErrorsTagged: TaggedValidationState[Unit] = ModifierValidator.accumulateErrorsTagged(aeSettings)

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = ModifierValidator.error(errorMessage)

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid = ModifierValidator.fatal(errorMessage)

  /** unsuccessful validation with a given error */
  def invalid(error: ModifierError): Invalid = ModifierValidator.invalid(error)

  /** successful validation */
  def success: Valid[Unit] = ModifierValidator.success

  val trueCondition: String => Boolean = _ => true
  val falseCondition: String => Boolean = _ => false

  "ModifierValidation" should "be able to succeed when failing fast" in {
    val result = failFast
      .validate(condition = true)(fatal(""))
      .result
    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "fail for missed checks" in {
    val result = failFast
      .validate(condition = false)(fatal(""))
      .result
    result shouldBe an[Invalid]
    result.errors should have size 1
  }

  it should "be able to succeed when accumulating errors" in {
    val result = accumulateErrors
      .validate(condition = true)(fatal(""))
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "support fail fast approach" in {
    val result = failFast
      .validate(condition = false)(fatal(""))
      .validate(condition = false)(fatal(""))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
  }

  it should "support error accumulation" in {
    val result = accumulateErrors
      .validate(condition = false)(error(""))
      .validate(condition = false)(error(""))
      .validate(condition = true)(fatal(""))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
  }

  it should "be lazy" in {
    var i = 0
    val result = failFast
      .validate(condition = false)(error(""))
      .validate(s"${i = 1}" == s"${()}")(error(""))
      .result
    result.isValid shouldBe false
    result shouldBe an[Invalid]
    i shouldBe 0
  }

  it should "support recoverable errors" in {
    val result = accumulateErrors
      .validate(condition = false)(error(""))
      .result

    result shouldBe an[Invalid]
    result.errors should have size 1
    all(result.errors) shouldBe a[RecoverableModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe false
  }

  it should "support fatal errors" in {
    val result = accumulateErrors
      .validate(condition = false)(fatal(""))
      .validate(condition = false)(error(""))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    exactly(1, result.errors) shouldBe a[MalformedModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe true
  }

  it should "support accumulating nesting" in {
    val result = accumulateErrors
      .validate(condition = false)(fatal(""))
      .validate {
        failFast
          .validate(condition = true)(fatal(""))
          .validate(condition = false)(fatal(""))
          .validate(condition = false)(fatal(""))
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "support fail fast nesting" in {
    val result = failFast
      .validate(condition = true)(fatal(""))
      .validate {
        accumulateErrors
          .validate(condition = true)(fatal(""))
          .validate(condition = false)(fatal(""))
          .validate(condition = false)(fatal(""))
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "fail fast while nesting" in {
    val errMsg = "Special for test"
    val result = failFast
      .validate(condition = false)(fatal(errMsg))
      .validate {
        accumulateErrors
          .validate(condition = false)(fatal(""))
          .validate(condition = false)(fatal(""))
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors.map(_.message) shouldBe Seq(errMsg)
  }

  it should "correctly check byte array equality" in {
    val len = 32
    val byte1 = 1.toByte
    val byte2 = 2.toByte
    val id = bytesToId(Array.fill(len)(byte1))
    val result = accumulateErrors
      .validateEqualIds(id, bytesToId(Array.fill(len)(byte2)))(_ => fatal("1"))
      .validateEqualIds(id, bytesToId(Array.fill(len)(byte1)))(_ => fatal(""))
      .validateEqualIds(id, bytesToId(Array.fill(len + 1)(byte1)))(_ => fatal("3"))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    result.errors.exists(_.message.startsWith("1")) shouldBe true
    result.errors.exists(_.message.startsWith("3")) shouldBe true
  }

  it should "correctly check equality" in {
    val result = accumulateErrors
      .validateEquals("123", "12" + "3")(_ => fatal(""))
      .validateEquals("123", "122")(_ => fatal("2"))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.exists(_.message.startsWith("2")) shouldBe true
  }

  it should "correctly check semantic validity" in {
    val result = accumulateErrors
      .validateSemantics(ModifierSemanticValidity.Invalid)(fatal("1"))
      .validateSemantics(ModifierSemanticValidity.Absent)(fatal(""))
      .validateSemantics(ModifierSemanticValidity.Unknown)(fatal(""))
      .validateSemantics(ModifierSemanticValidity.Valid)(fatal(""))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only "1"
  }

  it should "support `not` condition" in {
    val result = accumulateErrors
      .validateNot(condition = false)(fatal(""))
      .validateNot(condition = true)(fatal("3"))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only "3"
  }

  it should "carry payload" in {
    val data = 1L
    val result = accumulateErrors
      .payload(data)
      .validate(condition = true)(fatal(""))
      .result

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "replace payload" in {
    val initialData = "Hi there"
    val data = 1L
    val result = accumulateErrors
      .payload(initialData)
      .validate(condition = true)(fatal(""))
      .result(data)

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "fill payload from try" in {
    val data = "Hi there"
    val result = accumulateErrors
      .payload[String]("Random string")
      .validateTry(Try(data), _ => fatal(""))((_, d) => Valid(d))
      .result

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "return error when filling payload from failure" in {
    val errMsg = "error msg"
    val result = accumulateErrors
      .payload("Random string")
      .validateTry(Failure(new Error("Failed")), _ => fatal(errMsg))((_, d) => Valid(d))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors.exists(_.message.startsWith(errMsg)) shouldBe true
  }

  it should "switch failure payload type" in {
    val errMsg = "Failure 1"
    val stringFailure: ValidationState[String] = failFast
      .payload("Hi there")
      .pass(fatal(errMsg))

    val unitFailure: ValidationState[Unit] = stringFailure
      .pass(success)
      .pass(fatal(errMsg + "23"))

    val result = unitFailure.result
    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.payload shouldBe empty
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg
  }

  it should "validate optional for some" in {
    val expression = "123"
    val errMsg = "Error msg"
    val result = accumulateErrors
      .validateOrSkip[String](Some(expression))((_, _) => fatal(errMsg))
      .result

    result.isValid shouldBe false
    result.errors.map(_.message) should contain only errMsg
  }

  it should "skip optional validation for none" in {
    val result = accumulateErrors
      .validateOrSkip[String](None)((_, _) => fatal(""))
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  // Tagged mode

  "ModifierValidation" should "be able to succeed when failing fast (tagged mode)" in {
    val result = failFastTagged
      .validate(1, condition = true)
      .result
    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "skip deactivated checks (tagged mode)" in {
    val result = failFastTagged
      .validate(10, condition = false)
      .result
    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "fail for missed checks (tagged mode)" in {
    val result = failFastTagged
      .validate(2, condition = false)
      .result
    result shouldBe an[Invalid]
    result.errors should have size 1
  }

  it should "be able to succeed when accumulating errors (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validate(1, condition = true)
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "support fail fast approach (tagged mode)" in {
    val result = failFastTagged
      .validate(1, condition = false)
      .validate(3, condition = false)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
  }

  it should "support error accumulation (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validate(1, condition = false)
      .validate(3, condition = false)
      .validate(4, condition = true)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
  }

  it should "be lazy (tagged mode)" in {
    var i = 0
    val result = failFastTagged
      .validate(1, condition = false)
      .validate(3, s"${i = 1}" == s"${()}")
      .result
    result.isValid shouldBe false
    result shouldBe an[Invalid]
    i shouldBe 0
  }

  it should "support recoverable errors (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validate(5, condition = false)
      .result

    result shouldBe an[Invalid]
    result.errors should have size 1
    all(result.errors) shouldBe a[RecoverableModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe false
  }

  it should "support fatal errors (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validate(5, condition = false)
      .validate(1, condition = false)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    exactly(1, result.errors) shouldBe a[MalformedModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe true
  }

  it should "support accumulating nesting (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validate(1, condition = false)
      .validate {
        failFastTagged
          .validate(3, condition = true)
          .validate(4, condition = false)
          .validate(5, condition = false)
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "support fail fast nesting (tagged mode)" in {
    val result = failFastTagged
      .validate(1, condition = true)
      .validate {
        accumulateErrorsTagged
          .validate(2, condition = true)
          .validate(3, condition = false)
          .validate(4, condition = false)
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "fail fast while nesting (tagged mode)" in {
    val result = failFastTagged
      .validate(3, condition = false)
      .validate {
        accumulateErrorsTagged
          .validate(4, condition = false)
          .validate(5, condition = false)
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors.map(_.message) shouldBe Seq(errMsg3)
  }

  it should "correctly check byte array equality (tagged mode)" in {
    val len = 32
    val byte1 = 1.toByte
    val byte2 = 2.toByte
    val id = bytesToId(Array.fill(len)(byte1))
    val result = accumulateErrorsTagged
      .validateEqualIds(1, id, bytesToId(Array.fill(len)(byte2)))
      .validateEqualIds(4, id, bytesToId(Array.fill(len)(byte1)))
      .validateEqualIds(3, id, bytesToId(Array.fill(len + 1)(byte1)))
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    result.errors.exists(_.message.startsWith(errMsg1)) shouldBe true
    result.errors.exists(_.message.startsWith(errMsg3)) shouldBe true
  }

  it should "correctly check equality (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validateEquals(1, "123", "12" + "3")
      .validateEquals(3, "123", "122")
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.exists(_.message.startsWith(errMsg3)) shouldBe true
  }

  it should "correctly check semantic validity (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validateSemantics(1, ModifierSemanticValidity.Invalid)
      .validateSemantics(2, ModifierSemanticValidity.Absent)
      .validateSemantics(3, ModifierSemanticValidity.Unknown)
      .validateSemantics(4, ModifierSemanticValidity.Valid)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg1
  }

  it should "support `not` condition (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validateNot(1, condition = false)
      .validateNot(3, condition = true)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg3
  }

  it should "carry payload (tagged mode)" in {
    val data = 1L
    val result = accumulateErrorsTagged
      .payload(data)
      .validate(1, condition = true)
      .result

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "replace payload (tagged mode)" in {
    val initialData = "Hi there"
    val data = 1L
    val result = accumulateErrorsTagged
      .payload(initialData)
      .validate(1, condition = true)
      .result(data)

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "fill payload from try (tagged mode)" in {
    val data = "Hi there"
    val result = accumulateErrorsTagged
      .payload[String]("Random string")
      .validateTryFlatten(1, _ => Try(data), trueCondition)
      .result

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "return error when filling payload from failure (tagged mode)" in {
    val result = accumulateErrorsTagged
      .payload("Random string")
      .validateTryFlatten(1, _ => Failure(new Error("Failed")), trueCondition)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors.exists(_.message.startsWith(errMsg1)) shouldBe true
  }

  it should "aggregate payload from try (tagged mode)" in {
    val data1 = 100L
    val data2 = 50L
    val expected = data1 / data2
    val result = accumulateErrorsTagged
      .payload[Long](data1)
      .validateTryFlatten(1, v => Try(v / data2), _ => true)
      .result

    result.isValid shouldBe true
    result shouldBe Valid(expected)
    result.payload shouldBe Some(expected)
  }

  it should "return error when aggregating payload from failure (tagged mode)" in {
    val result = accumulateErrorsTagged
      .payload(1)
      .validateTryFlatten(1, _ => Failure(new Error("Failed")): Try[Int], _ => true)
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors.exists(_.message.startsWith(errMsg1)) shouldBe true
  }

  it should "switch failure payload type (tagged mode)" in {
    val errMsg = "Failure 1"
    val stringFailure: TaggedValidationState[String] = failFastTagged
      .payload("Hi there")
      .pass(fatal(errMsg))

    val unitFailure: TaggedValidationState[Unit] = stringFailure
      .pass(success)
      .pass(fatal(errMsg + "23"))

    val result = unitFailure.result
    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.payload shouldBe empty
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg
  }

  it should "validate optional for some (tagged mode)" in {
    val expression = "123"
    val result = accumulateErrorsTagged
      .validateOrSkipFlatten[String](1, Some(expression), falseCondition)
      .result

    result.isValid shouldBe false
    result.errors.map(_.message) should contain only errMsg1
  }

  it should "skip optional validation for none (tagged mode)" in {
    val result = accumulateErrorsTagged
      .validateOrSkipFlatten[String](1, None, falseCondition)
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  def constructValidationSettings(failFast: Boolean, map: Map[Short, (String => Invalid, Boolean)]
                                 ): TaggedValidationRules = {

    new TaggedValidationRules {

      override def getError(id: Short, details: String): Invalid = {
        map.get(id).map(_._1(details)).getOrElse(ModifierValidator.fatal("Unknown message"))
      }

      override def isActive(id: Short): Boolean = map.get(id).forall(_._2)

    }

  }
}
