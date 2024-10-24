package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Parallelism extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningOfLife.zip(favLang) // combines/zips in sequential

  // combine in parallel
  val combinedPar = meaningOfLife.zipPar(favLang) // combination is parallel

  /*
  - start each on fibers
  - what if one fails? the other should be interrupted
  - what if one is interrupted? the entire thing should be interrupted
  - what if the whole thing is interrupted? need to interrupt both effects
 */

  // try a zipPar combinator
  // hint: fork/join/await, interrupt
  def myZipPar[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,(A,B)] = {
    val exits = for {
      fiba <- zioa.fork
      fibb <- ziob.fork
      exita <- fiba.await
      exitb <- exita match
        case Exit.Success(value) => fibb.await
        case Exit.Failure(cause) => fibb.interrupt // if cause => failure/interrupt both case interrupt fiberB  //*** NOTE: interrupt returns UIO[EXIT[E,B]]
    } yield (exita, exitb)

    exits.flatMap {
      case(Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a, b)) // happy path
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause) // on of them failed
      case (Exit.Failure(cause), Exit.Success(_)) => ZIO.failCause(cause) // on of them failed
      case (Exit.Failure(c1), Exit.Failure(c2)) => ZIO.failCause(c1 && c2) // both fail    (&& combinator of causes)
    }
  }

  // parallel combinators
    // zipPar, zipWithPar

    // collectAllPar
  val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedValues: ZIO[Any, Nothing, Seq[Int]] = ZIO.collectAllPar(effects) // "traverse" Seq lai ZIO vitra(Inside Out)

  // foreachPar   (curried fxn)
  val printlnParallel = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  // reduceALlPar, mergeALlPar
  val sumPar = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _)

  /*
     - if all the effects succeed, all good
     - one effect fails => everyone else is interrupted, error is surfaced
     - one effect is interrupted => everyone else is interrupted, error = interruption (for the big expression)
     - if the entire thing is interrupted => all effects are interrupted
    */

  /**
   * Exercise: do the word counting exercise, using a parallel combinator
   */
  def countWords(path: String): UIO[Int] =
    ZIO.succeed{
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      source.close()
      nWords
    }

  def wordCountParallel(n: Int) =
    val effects = (1 to n)
      .map(i =>  s"src/main/resources/testfile_${i}")
      .map(path => countWords(path))

    // v1 - collectAllPar
    ZIO.collectAllPar(effects).map(_.sum)

    // v2 - mergeAllPar                                                   // ALL 3 does the same thing
    ZIO.mergeAllPar(effects)(0)(_ + _)

    // v3 - reduceAll Par
    ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)


  def run = wordCountParallel(10).debugThread
}
