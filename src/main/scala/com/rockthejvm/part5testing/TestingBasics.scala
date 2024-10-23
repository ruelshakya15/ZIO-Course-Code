package com.rockthejvm.part5testing

import zio.*
import zio.test.*
import zio.test.Assertion.{equalTo, hasMessage}  // NEED TO IMPORT THIS for TESTING


case class Person(name: String, age: Int) {
  def spellName: String = name.toUpperCase()
  def saySomething: UIO[String] = ZIO.succeed(s"Hi, I'm $name ")
}

object MyTestSpec extends ZIOSpecDefault {
  // fundamental method
  def spec = test("First Test") {
    val person = Person("Daniel", 99)

    // an assertion
    assert(person.spellName)(Assertion.equalTo("DANIEL"))
    // same
    assertTrue(person.spellName == "DANIEL")
    // must return an assertion
  }
}

// testing a ZIO effect
object MyFirstEffectTestSpec extends ZIOSpecDefault {
  def spec = test("First Effect Test") {
    val person = Person("Daniel", 101)
    assertZIO(person.saySomething)(Assertion.equalTo("Hi, I'm Daniel"))
    assertZIO(person.saySomething)(Assertion.assertion("should be a greeting")(gr => gr == "Hi, I'm Daniel"))
    assertZIO(person.saySomething)(Assertion.assertion("should be a greeting")(gr => gr == "Hi, I'm Daniel"))

    assertZIO(ZIO.fail("Error"))(Assertion.fails(Assertion.equalTo("Error")))
    assertZIO(ZIO.failCause(Cause.fail("Error")))(Assertion.failsCause(Assertion.equalTo(Cause.fail("Error"))))
                                                                  // ^^^ ***IMP Note: Assertion hatayera equalTo(Cause.fail("Error") eti matrai ni lekhna milcha cause Assertion. garisakeko cha agadi teskai fxn call huncha
    assertZIO(ZIO.die(new RuntimeException("Boom")))(Assertion.dies(hasMessage(equalTo("Boom"))))
    assertZIO(ZIO.interrupt)(Assertion.isInterrupted)
    assert(Left("Error"))(Assertion.isLeft(equalTo("Error")))




    // doesn't work with asserTrue
    /*
          Assertion examples:
          - Assertion.assertion => tests any truth value = the most general assertion
          - Assertion.equalTo => tests for equality
          - Assertion.fails/failsCause => expects the effect to fail with the EXACT failure/cause you specify
          - Assertion.dies => expects the effect to die with a Throwable, can run an assertion on that Throwable
          - Assertion.isInterrupted => validates an interruption
          - Specialized
            - isLeft/isRight for Either
            - isSome/isNone for Option
            - isSuccess/isFailure for Try
            - isEmpty/nonEmpty, contains, has* for iterables
            - isEmptyString/nonEmptyString, startsWithString, matchesRegex for Strings
         */
  }
}

object ASuiteSpec extends ZIOSpecDefault {
  def spec = suite("Full Suite of Tests") (
    // pass multiple test as arguments
    test ("simple test") {
      assertTrue(1 + 3 == 4)
    },
    test("a second test") {
      assertZIO(ZIO.succeed("Scala"))(Assertion.hasSizeString(Assertion.equalTo(5)) && Assertion.startsWithString("S"))
    },
    // sub-suites
    suite("a nested suite") (
      test("a nested test") {
        assert(List(1,2,3))(Assertion.isNonEmpty && Assertion.hasSameElements(List(1,2,3)))
      },
      test("another nested test") {
        assert(List(1,2,3).headOption)(Assertion.equalTo(Some(1)))
      },
      test("a failed nested test") {
        assertTrue(1 + 1 == 100)
      }
    )
  )
}