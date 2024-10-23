package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  // FINALIZERS
  def unsafeMethod(): Int = throw new RuntimeException("Not an int here for you!")
  val anAttempt = ZIO.attempt(unsafeMethod())

    // finalizers(they run no matter what even if ZIO fails/succeeds)
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("finalizer!").debugThread)
    // multiple finalizers    (finalizers run before error surfaced out)
  val attemptWith2Finalizers = attemptWithFinalizer.ensuring(ZIO.succeed("another finalizer!").debugThread)
    // .onInterrupt, .onError, . onDone, .onExit (eg of Finalizers)

  // Resource Lifecycle (finalizer manage it)
  class Connection(url: String) {
    def open() = ZIO.succeed(s"opening connection to $url...").debugThread
    def close() = ZIO.succeed(s"closing connection to $url").debugThread
  }

  object Connection{
    def create(url: String) = ZIO.succeed(new Connection(url))
  }

  val fetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield () // resource leak  (because before correctly closed interrupted)

  val correctFetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).ensuring(conn.close()).fork  // (preventing leaks) USE finalizer her to prevent leaks like this
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  // But Above way "TEDIOUS" so we use pattern below
  /*
   acquireRelease
    - acquiring cannot be interrupted
    - all finalizers are guaranteed to run
    - has "Scoped" dependency
   */                         // notice: Scope in rChannel   (provide auto by runtime unless specified explicitly)
  val cleanConnection: ZIO[Any & Scope, Nothing, Connection] = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close()) // finalizer
  val fetchWithResource = for {
    conn <- cleanConnection
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork  // DONT NEED TO WORRY ABOUT CLOSING CONN (DIFFERENT from above)
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  val fetchWithScopedResource = ZIO.scoped(fetchWithResource)  // .scoped eliminate scoped dependency in rchannel ZIO[Any & SCOPE, .., ...] => ZIO[Any .., ...]

  // acquireReleaseWith  (curried fxn 3 param)  (also no Scoped dependency)
  val cleanConnection_v2 = ZIO.acquireReleaseWith(
    Connection.create("rockthjvm.com") // acquire
  ) (
    _.close() // release
  ) (
    conn => conn.open() *> ZIO.sleep(300.seconds)  //use
  )

  val fetchWithResource_v2 = for{
    fib <- cleanConnection_v2.fork // acquire, release , "use" part executed once .fork is called
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield()

  /**
   * Exercises
   * 1. Use the acquireRelease to open a file, print all lines, (one every 100 millis), then close the file
   */
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def readLineByLine(scanner: Scanner): UIO[Unit] =
    if (scanner.hasNextLine)
      ZIO.succeed(scanner.nextLine()).debugThread *> ZIO.sleep(100.millis) *> readLineByLine(scanner)
    else
      ZIO.unit

  def acquireOpenFile(path: String) : UIO[Unit] =
    ZIO.succeed("opening file at $path").debugThread *>
      ZIO.acquireReleaseWith(
        openFileScanner(path)  //acquire
      )(
        scanner => ZIO.succeed(s"closing file at $path") *> ZIO.succeed(scanner.close())  // close
      )(
        readLineByLine
      )

        // OWN
  def acquireOpenFile_v2(path: String) : UIO[Unit] = {
    ZIO.acquireReleaseWith(openFileScanner(path))(x => ZIO.succeed(x.close())) { r =>
      def readLines: ZIO[Any, Nothing, Unit] =
        if (r.hasNext) {
          ZIO.succeed(r.nextLine()).debugThread *> ZIO.sleep(100.millis) *> readLines
        } else ZIO.succeed("Finished Reading").debugThread.unit

      readLines
    }
  } .onInterrupt(ZIO.succeed("I was Interrupted").debugThread)

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").debugThread.fork
    _ <- ZIO.sleep(2.seconds).debugThread *> fib.interrupt
  } yield ()

  // acquireRelease vs acquireReleaseWith   (Here when we nest multiple "acquireReleaseWith" code becomes complex to read)
  def conFromConfig(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close())){ scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close()){ conn =>
        conn.open() *> ZIO.never // .never returns UNIT and NEVER FINISHES
      }
    }

  // NESTED RESOURCES (so for nested Resources better if we use "acquireRelease" Instead
  def connFromConfig_v2(path: String): UIO[Unit] = ZIO.scoped {
    for {
      scanner <- ZIO.acquireRelease(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close()))
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close())                 // NOTE: VIDEO JASARI CLOSE GARDA CONNECTION close hunnda suspend huncha time vyaucha vane check out why it happens
      _ <- conn.open() *> ZIO.never
    } yield ()
  }

  def run = connFromConfig_v2("src/main/resources/connection.conf")
}
