package com.rockthejvm.part3concurrency

import zio.ZIOAppDefault
import zio.*
import com.rockthejvm.utils.*

import java.io.{File, FileReader, FileWriter}
import scala.io.Source // to debug and check thread that is executing

object Fibers extends ZIOAppDefault{
  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually

  val sameThreadIO = for { // sequential execution   (both run on same thread)
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol, lang)

  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork // change effect -> return value as Fiber       i.e ZIO[R, E, A]  ->  ZIO[R, Nothing, Fiber[E, A]]
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a fiber (***NOTE: .join takes a fiber instance not a Effect with a fiber)
  def runOnAnotherThread[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.join // .join will return another Effect which will block until fiber completed      ->  returns result(value/error) in form of zio = IO[E,A]
  } yield result

  // awaiting a fiber
  def runOnAnotherThread_v2[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.await // .await same as join but            -> returns Exit[E, A] instead of result directly
  } yield result match {  // ZIOErrorHandling.scala line no 96 ko herda huncha to understand dataStructure
    case Exit.Success(value) => s"Suceeded with $value"
    case Exit.Failure(cause) => s"failed with $cause"
  }

  // .poll - peek at the result of the fiber RIGHT NOW , without blocking
  val peekFiber = for {
    fib <- ZIO.attempt{
            Thread.sleep(1000)
            42
          }.fork
    result <- fib.poll  // return Option(Exit[E, A])
  } yield result

  // Compose fibers (.zip)
  val zippedFibers = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2) // *** (IMP) because .zip returns a Fiber not a ZIO
    tuple <- fiber.join
  } yield tuple

  // .orElse
  val chainedFibers = for {
    fiber1 <- ZIO.fail("not good!").debugThread.fork                  // NOTE: "not good" might not be printed first as fiber 1 & 2 executed concurrently by ZIO
    fiber2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
    fiber = fiber1.orElse(fiber2)   // also returns another fiber
    message <- fiber.join
  } yield message

  //**********************************************************2ND VIDEO fiber exercises

  /**
   * Exercises
   */
  // 1 - zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both
  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E,(A, B)]]= {
   val finalEffect = for {
     v1 <- fiber1.join
     v2 <- fiber2.join
   } yield (v1, v2)
    finalEffect.fork
  }

     // calling fxn
  val zippedFibers_v2 = for {
    fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple


  // exercise 1 - but with each fiber having diff Error(E1,E2) type
  def zipFibersGeneral[E,E1 <: E, E2 <: E, A, B](fiber1: Fiber[E1, A], fiber2: Fiber[E2, B]): ZIO[Any, Nothing, Fiber[E , (A, B)]] = {
    // same implementation
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  // 2 - same thing with orElse
  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] =
   fiber1.join.orElse(fiber2.join).fork  // .join return a ZIO effect and calling "orElse" on that

      // OWN (I think it is correct)
  def chainFibers_v2[E,A](fiber1: Fiber[E,A], fiber2: Fiber[E,A]): ZIO[Any, Nothing, Fiber[E,A]] = {
    val finalEffect = for{
      v1 <- fiber1.await
      v2 <- fiber2.join
    } yield v1 match
        case Exit.Failure(e) => v2
        case Exit.Success(value) => value
    finalEffect.fork
  }

      // calling function
  val chainedFibers_v2 = for {
    fiber1 <- ZIO.fail("not good!").debugThread.fork // NOTE: "not good" might not be printed first as fiber 1 & 2 executed concurrently by ZIO
    fiber2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
    fiber <- chainFibers(fiber1, fiber2)// also returns another fiber
    message <- fiber.join
  } yield message

  // 3 - distributing a task in between many fibers
  // spawn n fiber, count the n of words in each file,
  // the aggregate all the results together in on big number

  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) //  at most 2000 random words

    val content = ( 1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  // Solution:    ***IMP : This exercise good to understand fibers join and fork methods
    // part 1 - an effect which reads one file
    def countWords(path: String): UIO[Int] =
      ZIO.succeed{
        val source = scala.io.Source.fromFile(path)
        val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
        source.close()
        nWords
      }

    // part 2 - spin up fibers for all the files
    def wordCountParallel(n: Int): UIO[Int] =
      val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
        .map(i => s"src/main/resources/testfile_${i}") // paths
        .map(countWords) // list of effects
        .map(_.fork) // list of effects returning fibers
        .map((fiberEff: ZIO[Any, Nothing, Fiber[Nothing, Int]]) => fiberEff.flatMap(_.join)) // list of effects returning values (count of words)

      effects.reduce{ (zioa, ziob) =>
        for{
          a <- zioa
          b <- ziob
        } yield (a + b)
      }



  // OWN:    ( dnt know if its "efficient"[actually executed concurrently] word count is off by 1)
  val wordCountParallel_v2 = {
    val readFile = (path: String) => {
      val source = Source.fromFile(path)
      val readFileString = source.mkString    // acc to GPT inefficient as reading itself happens before any ZIO effects are run, meaning it's not leveraging ZIO's concurrency model effectively.
      source.close()
      readFileString
    }

    val fileSeq = ZIO.succeed((1 to 10).map(i => readFile(s"src/main/resources/testfile_${i}")))
    val finalEffect = for {
      result <- fileSeq.fork
      waitResult <- result.join
    } yield waitResult.map(x => x.split("\\s+").length).sum

    finalEffect
  }



  def run = { // wordCountParallel_v2.debugThread
    wordCountParallel(10).debugThread
    //chainedFibers.debugThread
    //zippedFibers_v2.debugThread

    //ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testfile_${i}")))
  }
}
