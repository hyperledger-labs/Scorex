package scorex.core.validation

import org.scalatest.{FlatSpec, Matchers}
import scorex.core.ModifierId
import scorex.core.consensus.ModifierSemanticValidity
import scorex.core.validation.ValidationResult._

class ValidationSpec extends FlatSpec with Matchers with ModifierValidation {

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

    result shouldBe an[Invalid]
    result.asInstanceOf[Invalid].errors should have size 1
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

    result shouldBe an[Invalid]
    result.asInstanceOf[Invalid].errors should have size 2
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
    val invalid = result.asInstanceOf[Invalid]
    invalid.isFatal shouldBe false
    invalid.errors should have size 1
    all(invalid.errors) shouldBe a[RecoverableModifierError]
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
    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.isFatal shouldBe true
    invalid.errors should have size 2
    exactly(1, invalid.errors) shouldBe a[MalformedModifierError]
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
    result shouldBe an[Invalid]
    result.asInstanceOf[Invalid].errors should have size 2
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
    result shouldBe an[Invalid]
    result.asInstanceOf[Invalid].errors should have size 2
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
    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.errors.map(_.message) shouldBe Seq(errMessage)
  }

  it should "correctly check byte array equality" in {
    val len = 32
    val byte1 = 1.toByte
    val byte2 = 2.toByte
    val id = ModifierId @@ Array.fill(len)(byte1)
    val differentBytesMsg = "Different bytes"
    val differentLengthMsg = s"Different length"
    val result = accumulateErrors
      .validateEquals(id, ModifierId @@ Array.fill(len)(byte1)) { detail =>
        fatal(s"Should never happen. $detail")
      }
      .validateEquals(id, ModifierId @@ Array.fill(len)(byte2)) { _ =>
        fatal(differentBytesMsg)
      }
      .validateEquals(id, ModifierId @@ Array.fill(len + 1)(byte1)) { _ =>
        fatal(differentLengthMsg)
      }
      .result
    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.errors should have size 2
    invalid.errors.map(_.message) should contain only(differentBytesMsg, differentLengthMsg)
  }

  it should "correctly check equality" in {
    val errMsg = "Error"
    val result = accumulateErrors
      .validateEquals("123", "12" + "3") { detail =>
        fatal(s"Should never happen. $detail")
      }
      .validateEquals("123", "122") { _ =>
        fatal(errMsg)
      }
      .result

    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.errors should have size 1
    invalid.errors.map(_.message) should contain only errMsg
  }

  it should "correctly check semantic validity" in {
    val result = accumulateErrors
      .validateSemantics(ModifierSemanticValidity.Valid) {
        fatal("Should never happen")
      }
      .validateSemantics(ModifierSemanticValidity.Invalid) {
        fatal(ModifierSemanticValidity.Invalid.toString)
      }
      .validateSemantics(ModifierSemanticValidity.Absent) {
        fatal(ModifierSemanticValidity.Absent.toString)
      }
      .validateSemantics(ModifierSemanticValidity.Unknown) {
        fatal(ModifierSemanticValidity.Unknown.toString)
      }
      .result

    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.errors should have size 3
    invalid.errors.map(_.message) should contain only(ModifierSemanticValidity.Invalid.toString,
                                                      ModifierSemanticValidity.Absent.toString,
                                                      ModifierSemanticValidity.Unknown.toString)
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

    result shouldBe an[Invalid]
    val invalid = result.asInstanceOf[Invalid]
    invalid.errors should have size 1
    invalid.errors.map(_.message) should contain only errMsg
  }

}
