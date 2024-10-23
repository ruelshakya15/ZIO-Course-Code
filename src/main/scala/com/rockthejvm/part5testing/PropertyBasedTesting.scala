package com.rockthejvm.part5testing

import zio._
import zio.test._
import com.rockthejvm.utils._

object PropertyBasedTesting extends ZIOSpecDefault {

  // PropertyBasedTesting used for "proofs"
  // for all x,y,z, we have (x + y) + z == x + (y + z)
  // shrinking -> Generators used shrinking to show us relatable small values that we can understand for a test passing or failing

  def spec = test("property-based-testing-basics") {
    check(Gen.int, Gen.int, Gen.int) { (x, y, z) => // check take variable number of arguments
      assertTrue(((x + y) + z) == (x - (y + z)))
    }
  }

  /*
    Property-based testing:
    "for all x,y,z,... we have Statement to be true."

    translation:
    check(a bunch of generators) { (a,b,c,d,...) =>
      assertions on the values generated
    }
   */
  // Gen[R,A]; R = "environment", A = value
  // Generator that produce => (values)
  val intGenerator = Gen.int
  val charGenerator = Gen.char // also alphaChar, alphaNumericChar, asciiChar, hexChar, printableChar etc
  val stringGenerator = Gen.string
  val cappedLengthStringGenerator = Gen.stringN(10)(Gen.alphaNumericChar)
  val constGenerator = Gen.const("Scala")
  val valuesGenerator = Gen.elements(1, 3, 4, 7, 9)
  val valuesIterableGenerator = Gen.fromIterable(1 to 1000)
  val uniformDoublesGenerator = Gen.uniform // select doubles between 0 and 1

  //  => (collections)
  val listGenerator = Gen.listOf(Gen.string) // unbounded list of strings
  val finiteSetGenerator = Gen.setOfN(10)(Gen.int) // sets of 10 integers

  //  => (option, either)
  val optionGenerator = Gen.option(Gen.int) // produce Option[Int]
  val eitherGenerator = Gen.either(Gen.string, Gen.int) // produce Either[String, Int]

  //  => (combinators)
  val zippedGenerator = Gen.int.zip(Gen.string) // produces (Int, String)
  val filteredGenerator = intGenerator.filter(_ % 3 == 0)
  val mappedGenerator = intGenerator.map(n => (1 to n).map(_ => 'a').mkString)
  val flatMappedGenerator = filteredGenerator.flatMap(l => Gen.stringN(l)(Gen.alphaNumericChar))

  // => (for-comprehension)
    // uuid are (8-4-4-12) letters
  val uuidGenerator = for {
    part1 <- Gen.stringN(8)(Gen.alphaNumericChar)
    part2 <- Gen.stringN(4)(Gen.alphaNumericChar)
    part3 <- Gen.stringN(4)(Gen.alphaNumericChar)
    part4 <- Gen.stringN(12)(Gen.alphaNumericChar)
  } yield s"$part1-$part2-$part3-$part4"

  // => (general)
  val randomGenerator = Gen.fromRandom(random => random.nextUUID) // using ZIO random
  val effectGenerator = Gen.fromZIO(ZIO.succeed(42)) // ZIO can be anything any API for own logic
  // lists of strings with the property that every string will have increasing length (starting from 0)
  val generalGenerator = Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))
}

object GenerationPlayground extends ZIOAppDefault {
  def run = {
    val generalGenerator = Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar))) // since unfoldGen has Requirement "Sized" we need to pass it in line 72
    val generatedListsZIO = generalGenerator.runCollectN(100)
    val generatedListsZIO_v2 = generatedListsZIO.provideLayer(Sized.live(50)) // generate 1 element by default ->  "Sized.default"
    generatedListsZIO_v2.debugThread
  }
}
