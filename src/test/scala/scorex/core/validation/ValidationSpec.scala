package scorex.core.validation

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.bytesToId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationResult._

import scala.util.{Failure, Try}

class ValidationSpec extends FlatSpec with Matchers with ModifierValidator with ScorexEncoding {

  "ModifierValidation" should "be able to succeed when failing fast" in {
    val result = failFast
      .validate(condition = true) {
        fatal("Should never happen")
      }
      .result
    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "be able to succeed when accumulating errors" in {
    val result = accumulateErrors
      .validate(condition = true) {
        fatal("Should never happen")
      }
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "support fail fast approach" in {
    val result = failFast
      .validate(condition = false) {
        fatal("Stop validation here")
      }
      .validate(condition = false) {
        fatal("This should be skipped")
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
  }

  it should "support error accumulation" in {
    val result = accumulateErrors
      .validate(condition = false) {
        fatal("First validation failure")
      }
      .validate(condition = false) {
        fatal("Second validation failure")
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
  }

  it should "be lazy" in {
    var i = 0
    val result = failFast
      .validate(condition = false) {
        fatal("Stop validation here")
      }
      .validate(s"${i = 1}" == s"${()}") {
        i = 2
        fatal(s"This should be skipped")
      }
      .result
    result.isValid shouldBe false
    result shouldBe an[Invalid]
    i shouldBe 0
  }

  it should "support recoverable errors" in {
    val result = accumulateErrors
      .validate(condition = false) {
        error("Could be recovered")
      }
      .result

    result shouldBe an[Invalid]
    result.errors should have size 1
    all(result.errors) shouldBe a[RecoverableModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe false
  }

  it should "support fatal errors" in {
    val result = accumulateErrors
      .validate(condition = false) {
        error("Could be recovered")
      }
      .validate(condition = false) {
        fatal("Could not be recovered")
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    exactly(1, result.errors) shouldBe a[MalformedModifierError]
    result.asInstanceOf[Invalid].isFatal shouldBe true
  }

  it should "support accumulating nesting" in {
    val result = accumulateErrors
      .validate(condition = false) {
        fatal("First error")
      }
      .validate {
        failFast
          .validate(condition = true) {
            fatal("Should never happen")
          }
          .validate(condition = false) {
            fatal("Second error")
          }
          .validate(condition = false) {
            fatal("This error should be skipped")
          }
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "support fail fast nesting" in {
    val result = failFast
      .validate(condition = true) {
        fatal("Should never happen")
      }
      .validate {
        accumulateErrors
          .validate(condition = true) {
            fatal("Should never happen")
          }
          .validate(condition = false) {
            fatal("First error")
          }
          .validate(condition = false) {
            fatal("Second error")
          }
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors should have size 2
  }

  it should "fail fast while nesting" in {
    val errMessage = "Error"
    val result = failFast
      .validate(condition = false) {
        fatal(errMessage)
      }
      .validate {
        accumulateErrors
          .validate(condition = false) {
            fatal("First error, should not achieve this")
          }
          .validate(condition = false) {
            fatal("Second error, should not achieve this")
          }
          .result
      }
      .result

    result.isValid shouldBe false
    result.errors.map(_.message) shouldBe Seq(errMessage)
  }

  it should "correctly check byte array equality" in {
    val len = 32
    val byte1 = 1.toByte
    val byte2 = 2.toByte
    val id = bytesToId(Array.fill(len)(byte1))
    val differentBytesMsg = "Different bytes"
    val differentLengthMsg = s"Different length"
    val result = accumulateErrors
      .validateEqualIds(id, bytesToId(Array.fill(len)(byte1))) { detail =>
        fatal(s"Should never happen. $detail")
      }
      .validateEqualIds(id, bytesToId(Array.fill(len)(byte2))) { _ =>
        fatal(differentBytesMsg)
      }
      .validateEqualIds(id, bytesToId(Array.fill(len + 1)(byte1))) { _ =>
        fatal(differentLengthMsg)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 2
    result.errors.map(_.message) should contain only(differentBytesMsg, differentLengthMsg)
  }

  it should "correctly check equality" in {
    val errMsg = "Error"
    val result = accumulateErrors
      .validateEquals("123")("12" + "3") { detail =>
        fatal(s"Should never happen. $detail")
      }
      .validateEquals("123")("122") { _ =>
        fatal(errMsg)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg
  }

  it should "correctly check semantic validity" in {
    val result = accumulateErrors
      .validateSemantics(ModifierSemanticValidity.Valid) {
        fatal("Should never happen")
      }
      .validateSemantics(ModifierSemanticValidity.Absent) {
        fatal(ModifierSemanticValidity.Absent.toString)
      }
      .validateSemantics(ModifierSemanticValidity.Unknown) {
        fatal(ModifierSemanticValidity.Unknown.toString)
      }
      .validateSemantics(ModifierSemanticValidity.Invalid) {
        fatal(ModifierSemanticValidity.Invalid.toString)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only ModifierSemanticValidity.Invalid.toString
  }

  it should "support `not` condition" in {
    val errMsg = "Error"
    val result = accumulateErrors
      .validateNot(condition = false) {
        fatal("Should never happen")
      }
      .validateNot(condition = true) {
        fatal(errMsg)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors should have size 1
    result.errors.map(_.message) should contain only errMsg
  }

  it should "carry payload" in {
    val data = 1L
    val result = accumulateErrors
      .payload(data)
      .validate(condition = true)(fatal("Should never happen"))
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
      .validate(condition = true)(fatal("Should never happen"))
      .result(data)

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "fill payload from try" in {
    val data = "Hi there"
    val result = accumulateErrors
      .payload("Random string")
      .validateTry(Try(data), e => fatal(s"Should never happen $e")) {
        case (validation, str) => validation.result(str)
      }
      .result

    result.isValid shouldBe true
    result shouldBe Valid(data)
    result.payload shouldBe Some(data)
  }

  it should "return error when filling payload from failure" in {
    val errMsg = "Should not take payload from failure"
    val result = accumulateErrors
      .payload("Random string")
      .validateTry(Failure(new Error("Failed")), _ => fatal(errMsg)) {
        case (validation, str) => validation.result(str)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors.map(_.message) should contain(errMsg)
  }

  it should "aggregate payload from try" in {
    val data1 = 100L
    val data2 = 50L
    val expected = data1 / data2
    val result = accumulateErrors
      .payload(data1)
      .validateTry(Try(data2), e => fatal(s"Should never happen $e")) {
        case (validation, data) => validation.map(_ / data)

      }
      .result

    result.isValid shouldBe true
    result shouldBe Valid(expected)
    result.payload shouldBe Some(expected)
  }

  it should "return error when aggregating payload from failure" in {
    val errMsg = "Should not take payload from failure"
    val result = accumulateErrors
      .payload(1)
      .validateTry(Failure(new Error("Failed")): Try[Int], _ => fatal(errMsg)){
        case (validation, data) => validation.map(_ / data)
      }
      .result

    result.isValid shouldBe false
    result shouldBe an[Invalid]
    result.errors.map(_.message) should contain(errMsg)
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
    val cnt = expression.length
    val result = accumulateErrors
      .validateOrSkip(Some(expression)) { (validation, expr) =>
        validation.validate(expr.length == cnt)(fatal("Should never happen"))
      }
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

  it should "skip optional validation for none" in {
    val result = accumulateErrors
      .validateOrSkip(None) { (validation, _) =>
        validation.validate(condition = false)(fatal("Should never happen"))
      }
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid[_]]
  }

}
