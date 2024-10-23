package com.rockthejvm.part5testing

import zio._
import zio.test._
import zio.test.TestAspect._

object SimpleDependencySpec extends ZIOSpecDefault {
  def spec = test("simple dependency") {
    val aZIO: ZIO[Int, Nothing, Int] = ZIO.succeed(42)
    // can also do "aZIO.provide(ZLayer.succeed(10))" instead of at the end
    assertZIO(aZIO)(Assertion.equalTo(42))
  }.provide(ZLayer.succeed(10))
}

// example: a user survey application processing user data, fetching data from a database
object TestingBusinessLogicSpec extends ZIOSpecDefault {

  // "dependency"
  abstract class Database[K, V] {
    def get(key: K): Task[V]

    def put(key: K, value: V): Task[Unit]
  }

  object Database {
    def create(url: String): UIO[Database[String, String]] = ??? // the REAL thing
  }

  // logic under test
  def normalizeUsername(name: String): UIO[String] = ZIO.succeed(name.toUpperCase())

  val mockedDatabase = ZIO.succeed(new Database[String, String] {

    import scala.collection.mutable

    val map = mutable.Map[String, String]()

    override def get(key: String): Task[String] = ZIO.attempt(map(key))

    override def put(key: String, value: String): Task[Unit] = ZIO.succeed(map += (key -> value))
  })

  //testing
  def spec = suite("A user survey application should ....")(
    test("normalize user names") {
      val surveyPreliminaryLogic = for {
        db <- ZIO.service[Database[String, String]]
        _ <- db.put("123", "Daniel")
        username <- db.get("123")
        normalized <- normalizeUsername(username)
      } yield normalized

      assertZIO(surveyPreliminaryLogic)(Assertion.equalTo("DANIEL"))
    }

  ).provide(ZLayer.fromZIO(mockedDatabase))
}

/*
  built-in test services
  - console
  - random
  - clock
  - system
 */

object DummyConsoleApplication {
  def welcomeUser(): Task[Unit] = for {
    _ <- Console.printLine("Please enter your name...")
    name <- Console.readLine("")
    _ <- Console.printLine(s"Welcome, $name!")
  } yield ()
}

object BuiltInTestServiceSpec extends ZIOSpecDefault {
  def spec = suite("Checking built-in test services") (
    test("ZIO console application") {
      val logicUnderTest: Task[Vector[String]] = for {
        _ <- TestConsole.feedLines("Daniel") // need to feedLines before we call .welcomeUser or else app will block
        _ <- DummyConsoleApplication.welcomeUser()
        output <- TestConsole.output
      } yield output.map(_.trim)

      assertZIO(logicUnderTest)(Assertion.hasSameElements(List("Please enter your name...","", "Welcome, Daniel!")))
    },
    test("ZIO clock") {
      val parallelEffect = for {
        fiber <- ZIO.sleep(5.minutes).timeout(1.minute).fork  //tries to execute the ZIO.sleep(5.minutes) but will give up after 1 minute if it hasn't completed. If the ZIO.sleep effect doesn't complete in 1 minute, this will return None.
        _ <- TestClock.adjust(1.minute) // manipulate passage of time
        result <- fiber.join
      } yield result

      assertZIO(parallelEffect)(Assertion.isNone)
    },
    test("ZIO random") {
      val effect = for {
        _ <- TestRandom.feedInts(3,4,1,2)
        value <- Random.nextInt // this is ZIO-built in random but not it uses the above "TestRandom's" implementation
      } yield value

      assertZIO(effect)(Assertion.equalTo(3))
    }
  )
}

/*
  Test aspects
 */
object AspectsSpec extends ZIOSpecDefault {
  def computeMeaningOfLife: UIO[Int] =
    (ZIO.sleep(2.seconds)) *> ZIO.succeed(42)

  def spec = suite("Testing Aspects") (
    test("timeout aspect") {
      val effect = for {
        molFib <- computeMeaningOfLife.fork
        _ <- TestClock.adjust(3.seconds)
        v <- molFib.join
      } yield v

      assertZIO(effect)(Assertion.equalTo(42))
    } @@ timeout (10.millis) @@ eventually @@ diagnose(1.second) @@ timed // if test doesnt complete in 10 sec interrupted  // NEED TO import zio.test.TestAspect._
    /*
      Aspects (@@) :
      - timeout(duration)
      - eventually - retries until successful
      - nonFlaky(n) - repeats n times, stops at first failure
      - repeats(n) - same
      - retries(n) - retries n times, stops at first success
      - debug - prints everything in the console
      - silent - prints nothing
      - diagnose(duration) - if fails within duration explanation produced by fiber dumping
      - parallel/sequential (aspects of a SUITE)
      - ignore
      - success - fail all ignored tests
      - timed - measure execution time
      - before/beforeAll + after/afterAll
     */
  ) @@ parallel
}
