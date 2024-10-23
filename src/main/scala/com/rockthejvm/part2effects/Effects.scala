package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.util.Try

object Effects { //*** WATCH VIDEO END NOTES FOR SUMMARY

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b: Int): Int = a + b

  //  1) LOCAL REASONING - type signature describes the kind of computation that will be performed
  //  2) REFERENTIAL TRANSPARENCY(RT) - ability to replace an expression with the value that it evaluates to
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // not all expression are RT
  val resultOfPrinting: Unit = println("Learning ZIO")
  val resultOfPrinting_V2: Unit = () // not the same

  // example 2: changing a variable
  var anInt = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingInt_v2: Unit = () // not the same program

  // side effects are inevitable
  /*
    Effect properties( from above)
    - 1) LOCAL REASONING - type signature describes the kind of computation that will be performed
    - 2) REFERENTIAL TRANSPARENCY(RT) - ability to replace an expression with the value that it evaluates to
    - if side effects are required, construction must be separate from the EXECUTION
   */

  /*
    Example: Option == possibly absent values
    - type signature describes the kind of computation = a possibly absent value
    - type signature says that the computation returns an A, if the computation does produce something
    - no side effects are needed

    => Option is an effect
   */
  val anOption: Option[Int] = Option(42)

  /*
    Example 2: Future
    - describes an asynchronous computation
    - produces a value of type A,if it finishes and it's successful
    - side effects are required, construction is NOT SEPARATE from execution
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    Example 3: MyIO
    - describes a computation which might perform side effects
    - produces values of type A if the computation is successful
    - side effects are required , construction IS SEPARATE From execution

    My IO IS AN EFFECT!
   */
  // TODO: WATCH SCALA3 version of MONADS Video to better understand below

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effect")
    42
  })

  /***************************************************************************************************
   * Exercises - create some IO which
   *  1. measure the current time of the system
   *  2. measure the duration of a computation
   *    - use exercise 1
   *    - use map/flatMap combinations of MyIO
   *  3. read something from the console
   *  4. print something to the console (e.g. "what's your name"), then read, then print a welcome message
   */

  // 1
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = for{
    startTime <- currentTime
    result <- computation
    endTime <- currentTime
  } yield (endTime - startTime, result)

  // V2 to better understand above
  def measure_v2[A](computation: MyIO[A]): MyIO[(Long, A)] ={
    // *** IMP(refactoring done here in VIDEO to show how easy is it for "EFFECTS" to be refactored safely and W/O changing behaviour of program
    // [ LOOK AT VIDEO TOO MUCH REFACTORING DONE HERE!!!]
    MyIO{ () =>
      val startTime = System.currentTimeMillis()
      val result = computation.unsafeRun()
      val endTime = System.currentTimeMillis()
      (endTime - startTime, result)
    }
  }

  def demoMeasurement(): Unit = {
    val computation = MyIO(() => {
      println("Crunching numbers....")
      Thread.sleep(1000)
      println("Done!")
      42
    })

    println(measure(computation).unsafeRun())
    println(measure_v2(computation).unsafeRun())
  }


  // 3
  val readLine: MyIO[String] = MyIO(() => scala.io.StdIn.readLine())
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  //  4
  val program = for {
    _ <- putStrLn("Whats your name?")
    name <- readLine
    _ <- putStrLn(s"Welcome to Rock the JVM, $name!")
  } yield ()

  /*
    NOTE: MyIO[] data structure is a bridge between imperative & pure functional programming
    The above code block even if it may seem different VERY SIMILAR TO IMPERATIVE PROGRAMMING: like

    println(what is your name)
    name = readLine()
    println(hello, $name)
   */

  /**
   *
   *  A simplified ZIO
   */

  case class MyZIO[-R, +E, +A](unsafeRun: (R) => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }

  def main(args: Array[String]): Unit = {
   anIOWithSideEffects.unsafeRun() // when only we call(invoke) "unsafeRun"  something printed to console
   demoMeasurement()

    program.unsafeRun()
  }
}
