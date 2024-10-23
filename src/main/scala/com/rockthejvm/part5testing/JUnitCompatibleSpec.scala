package com.rockthejvm.part5testing

import com.rockthejvm.part5testing.ASuiteSpec.{suite, test}
import zio.*
import zio.test.*

class JUnitCompatibleSpec extends zio.test.junit.JUnitRunnableSpec {

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
