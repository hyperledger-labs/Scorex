package scorex.core.validation

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.ModifierId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.validation.ValidationResult._
import scorex.crypto.encode.{Base16, BytesEncoder}

class ValidationSpec extends FlatSpec with Matchers with ModifierValidator {

  override implicit val encoder: BytesEncoder = Base16


  "ModifierValidation" should "be able to succeed when failing fast" in {
    val result = failFast
      .validate(condition = true) {
        fatal("Should never happen")
      }
      .result
    result.isValid shouldBe true
    result shouldBe a[Valid]
  }

  it should "be able to succeed when accumulating errors" in {
    val result = accumulateErrors
      .validate(condition = true) {
        fatal("Should never happen")
      }
      .result

    result.isValid shouldBe true
    result shouldBe a[Valid]
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
    val id = ModifierId @@ Array.fill(len)(byte1)
    val differentBytesMsg = "Different bytes"
    val differentLengthMsg = s"Different length"
    val result = accumulateErrors
      .validateEqualIds(id, Array.fill(len)(byte1)) { detail =>
        fatal(s"Should never happen. $detail")
      }
      .validateEqualIds(id, Array.fill(len)(byte2)) { _ =>
        fatal(differentBytesMsg)
      }
      .validateEqualIds(id, Array.fill(len + 1)(byte1)) { _ =>
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
    result.errors.map(_.message) should contain only (ModifierSemanticValidity.Invalid.toString)
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
}
