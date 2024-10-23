package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

object Promises extends ZIOAppDefault {

  // Promise: primitive , blocked until another ZIO completes it, never become empty or modified again.             (Whenever we need to synchronize a fiber with another fiber, we can use Promise)
  val aPromise: ZIO[Any, Nothing, Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  // await - block the fiber until the promise has a value
  val reader = aPromise.flatMap { promise =>
    promise.await
  }

  // succeed, fail, complete  promises
  val writer = aPromise.flatMap { promise =>
    promise.succeed(42)
    // promise.complete()   // ".complete" takes a zio[E, A] with same sig as promise[E, A]
  }

  def demoPromise(): Task[Unit] = {
    // producer - consumer problem
    def consumer(promise: Promise[Throwable, Int]) = for {
      _ <- ZIO.succeed("[consumer] waiting for result ... ").debugThread
      mol <- promise.await
      _ <- ZIO.succeed(s"[consumer] I got the result: $mol").debugThread
    } yield ()

    def producer(promise: Promise[Throwable, Int]) = for {
      _ <- ZIO.succeed("[producer] crunching numbers ... ").debugThread
      _ <- ZIO.succeed(3.seconds)
      _ <- ZIO.succeed(s"[producer] complete").debugThread
      mol <- ZIO.succeed(42)
      _ <- promise.succeed(mol)
    } yield ()

    for {
      promise <- Promise.make[Throwable, Int]
      _ <- consumer(promise) zipPar producer(promise)
    } yield ()
  }

  /*  WHY ZIO "Promises" (Benefits) ?
      - purely functional block on a fiber until you get a signal from another fiber
      - waiting on a value which may not yet be available, without thread starvation
      - inter-fiber communication
     */

  // simulate downloading from multiple parts
  val fileParts = List("I", " love S", "cala", " with pure FP an", "d ZIO! <EOF>")

  def downloadFileWithRef(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String]): Task[Unit] = // not used "collectALLParDiscard" to simulate sequential download
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          ZIO.succeed(s"got '$part'").debugThread *> ZIO.sleep(1.second) *> contentRef.update(_ + part)
        }
      )

    def notifyFileComplete(contentRef: Ref[String]): Task[Unit] = for { //***IMP: without use of promise in this code block we are busy waiting for EOF, waiting checking waiting checking and so on.....
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) ZIO.succeed("File download complete.").debugThread
      else ZIO.succeed("downloading ... ").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref.make("")
      _ <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }

  def downloadFileWithRefPromise(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] = // not used "collectALLParDiscard" to simulate sequential download
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          for {
            _ <- ZIO.succeed(s"got '$part'").debugThread
            _ <- ZIO.sleep(1.second)
            file <- contentRef.updateAndGet(_ + part)
            _ <- if (file.endsWith("<EOF>")) promise.succeed(file) else ZIO.unit
          } yield ()
        }
      )

    def notifyFileComplete(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] = for {
      _ <- ZIO.succeed("downloading...").debugThread
      file <- promise.await
      _ <- ZIO.succeed(s"file download complete: $file").debugThread
    } yield ()

    for {
      contentRef <- Ref.make("")
      promise <- Promise.make[Throwable, String]
      _ <- downloadFile(contentRef, promise) zipPar notifyFileComplete(contentRef, promise)
    } yield ()
  }

  //***IMP: without use of promise in this code block we are busy waiting for EOF, waiting checking waiting checking and so on.....

  //*********************************************************************2ND VIDEO************************************************************************

  /**
   * Exercises
   * 1. Write a simulated "egg boiler" with two ZIOs
   *  - one increments a counter every 1s
   *  - one waits for the counter to become 10, after which it will "ring a bell"
   *
   * 2. Write a "race pair"
   *  - use a Promise which can hold an Either[exit for A, exit for B]
   *  - start a fiber for each ZIO
   *  - on completion (with any status), each ZIO needs to complete that Promise
   *    (hint: use a finalizer)
   *  - waiting on the Promise's value can be interrupted!
   *  - if the whole race is interrupted, interrupt the running fibers
   */

  // 1 - Ans
  def eggBoiler(): UIO[Unit] = {
    def eggReady(signal: Promise[Nothing, Unit]) = for {
      _ <- ZIO.succeed("Egg boiling on some other fiber, waiting...").debugThread
      _ <- signal.await
      _ <- ZIO.succeed("EGG READY!").debugThread
    } yield ()

    def tickingClock(ticks: Ref[Int], signal: Promise[Nothing, Unit]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- ZIO.succeed(count).debugThread
      _ <- if (count >= 10) signal.succeed(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      ticks <- Ref.make(0)
      signal <- Promise.make[Nothing, Unit]
      _ <- eggReady(signal) zipPar tickingClock(ticks, signal)
    } yield ()
  }

  // 2 - Ans
  def racePair[R, E, A, B](zioa: => ZIO[R, E, A], ziob: ZIO[R, E, B]):
  ZIO[R, Nothing, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] =
    ZIO.uninterruptibleMask { restore =>
      for {
        promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
        fibA <- zioa.onExit(exitA => promise.succeed(Left(exitA))).fork
        fibB <- ziob.onExit(exitB => promise.succeed(Right(exitB))).fork

        result <- restore(promise.await).onInterrupt{  // if Interrupted interrupt both a & b
          for {
            interruptA <- fibA.interrupt.fork
            interruptB <- fibB.interrupt.fork
            _ <- interruptA.join
            _ <- interruptB.join      // Joining the fiber after interrupting it ensures that the fiber is properly cleaned up and avoids potential resource leaks
          } yield ()
        }
      } yield result match
        case Left(exitA) => Left(exitA, fibB)
        case Right(exitB) => Right(fibA, exitB)
    }


    // OWN - mistakes: haven't interrupted fiber a & b , and haven't enclose "promise.await" in on Interrupt to interrupt a & b
      // also Note: ".orDie" called to cast Throwable in eChannel -> Nothing ;
        // dont need to do this in ABOVE SOLN as in line 145  Promise.make[Nothing, Either[Exit[E,A], Exit[E,B]]] direct "Nothing" is called in eChannel
  def racePair_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: => ZIO[R, E, B]):
  ZIO[R, Nothing, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] = {
    val promiseEffect = Promise.make[Throwable, Either[Exit[E, A], Exit[E, B]]]

    ZIO.uninterruptibleMask { restore =>
      val endResult = for {
        promise <- promiseEffect
        fiba <- restore(zioa).fork
        fibb <- restore(ziob).fork
        _ <- fiba.join.onExit(exita => promise.succeed(Left(exita))).fork // NOTE: eta ni ".fork" so both execute on diff fibers
        _ <- fibb.join.onExit(exitb => promise.succeed(Right(exitb))).fork
        pResult <- promise.await
      } yield pResult match
        case Left(valueA) => Left((valueA, fibb))
        case Right(valueB) => Right(fiba, valueB)

      endResult.orDie
    }
  }

  val demoRacePair = {
    val zioa = ZIO.sleep(1.second).as(1).onInterrupt(ZIO.succeed("first interrupted").debugThread)
    val ziob = ZIO.sleep(2.second).as(2).onInterrupt(ZIO.succeed("second interrupted").debugThread)

    val pair = racePair(zioa, ziob)

    pair.flatMap {
      case Left(exitA, fibB) => fibB.interrupt *> ZIO.succeed("first won").debugThread *> ZIO.succeed(exitA).debugThread
      case Right(fibA, exitB) => fibA.interrupt *> ZIO.succeed("second won").debugThread *> ZIO.succeed(exitB).debugThread
    }
  }


  def run = demoRacePair
}
