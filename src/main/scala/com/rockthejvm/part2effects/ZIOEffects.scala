package com.rockthejvm.part2effects
import zio.*

import scala.io.StdIn

object ZIOEffects {

  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  // failure
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  // suspension/ delay in construction of another effect
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife) // suspension argument may result in exception so type "Throwable" added

  // map + flatmap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
  // for comprehensions
  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name"))
    name <- ZIO.succeed(StdIn.readLine()) // all this similar to MyIO[] exercise in Effects Chapter
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  // A LOT of combinators
  // zip, zipWith
  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   *
   * Type alias of ZIOs ( to simple ZIO[..] signature )
   */
  // UIO[A] = ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  // less used Type alias[ REQUIRES "requirement"] ->
  // a)
  // URIO[R,A] = ZIO[R,Nothing,A] - cannot fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)

  // b)
  // RIO[R,A] = ZIO[R, Throwable, A] - can fail with Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  //  Task[A] = ZIO[Any, Throwable, A] - no requirement, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89) // Because we use concept on variance +T,.. we can substitute Task[] with ZIO.succeed()
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))


  //  IO[E, A] = ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")


  /**
   *
   * Exercises
   */

  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa.flatMap(a => ziob.map(b => b))
  // for-comprehension
  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = for {
    a <- zioa
    b <- ziob
  } yield b
  // built-into ZIO
  def sequenceTakeLast_v3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob

  // 2 - sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    ziob.flatMap(b => zioa.map(a => a))
  // built-into ZIO
  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa <* ziob


  // 3 - run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap(_ => runForever(zio))

  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever_v2(zio) // same

  val endlessLoop = runForever{
    ZIO.succeed{
      println("running .... ")
      Thread.sleep(1000)
    }
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.map(_ => value)
  // built-in ZIO
  def convert_v2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.as(value)

  // 5 - discard the value of a ZIO to Unit
  // OWN
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.map(_ => println("this zio is useless"))
  //  calling above method
  def asUnit_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    convert_v2(zio, ())
  // built-in ZIO
  def asUnit_v3[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.unit

  // 6 - recursion [ ZIO Is optimized for tail recursion and prevents STACK OVERFLOW EXCEPTION in most cases]
  def sum(n: Int): Int =
    if( n == 0) 0
    else n + sum(n - 1) // will crash at sum(20000) big no

  def sumZIO(n: Int): UIO[Int] =
    if(n == 0) ZIO.succeed(0)
    else for{
      current <-  ZIO.succeed(n)
      prevSum <- sumZIO(n - 1)
    } yield current + prevSum

  // OWN( can use map flatmap instead)
  def sumZIO_v2(n: Int): UIO[Int] =
    if  (n == 0) ZIO.succeed(0)
    else sumZIO_v2(n - 1).map(x => n + x)

  // 7 - fibonacci
  // hint: use ZIO.suspend / ZIO.suspendSucceed
  def fibo(n: Int): BigInt =
    if (n <= 2) 1
    else fibo(n - 1) + fibo(n - 2)

  def fiboZIO(n: Int): UIO[BigInt] ={
    if ( n <= 2) ZIO.succeed(1)
    else for {
      last <- ZIO.suspendSucceed(fiboZIO(n - 1)) // prevents SYS from crashing
      prev <- fiboZIO(n - 2)
    } yield last + prev
  }

  /*
    PROBLEM with above code W/O using "suspendSucceed"
      fiboZIO(n-1).flatMap(last => fiboZIO(n-2).map(prev => last  + prev)))
      ^^ Here this needs to be evaluated for 199999 and then 199998... so we need to delay it
   */

  // OWN( ***I think this approach doesn't cause stackOverflow)[MIGHT BE WRONG]
  def fiboZIO_v2(n: Int): UIO[BigInt] = {
    def fiboHelper(count: Int, num: BigInt, acc: BigInt): BigInt = {
      if (count == 0) acc
      else fiboHelper(count - 1, acc , acc + num)
    }
    ZIO.suspendSucceed(ZIO.succeed(fiboHelper(n,1,1)))
  }


  def main(args: Array[String]): Unit = { //***(FOR this version of ZIO) -> use implicit instead of given
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty
    Unsafe.unsafeCompat { implicit u: Unsafe => // *** ERROR : [DIFF THAN VIDEO because it didnt work] 1) eta type parameter ra implicit vanerai toknu parcha maybe because of defn mai implicit vayera
      val mol = runtime.unsafe.run(meaningOfLife)                                                   // 2) ani "unsafeCompat" instead of "unsafe"
      println(mol)
    }

    Unsafe.unsafeCompat{ implicit u: Unsafe =>
      val firstEffect = ZIO.succeed{
        println("computing first effect ....")
        Thread.sleep(1000)
        1
      }

      val secondEffect = ZIO.succeed {
        println("computing second effect ....")
        Thread.sleep(1000)
        2
      }

      val first = ZIO.succeed(1)
      val first2 = ZIO.succeed(1)

      println(runtime.unsafe.run(fiboZIO(20)))

    }





    // NOTE: if you want to use given use like this
    //    given trace: Trace = Trace.empty
    //    Unsafe.unsafeCompat { (u: Unsafe) =>
    //      given uns: Unsafe = u
    //      ..
    //    ............
    //    }
  }
}
