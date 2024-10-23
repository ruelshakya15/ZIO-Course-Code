package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Interruptions extends ZIOAppDefault{

  val zioWithTime =
    (
    ZIO.succeed("Starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
      ZIO.succeed(42).debugThread
      )
      .onInterrupt(ZIO.succeed("I was interrupted!").debugThread)
        // onInterrupt, onDone fxn can be used

  val interruption = for {
    fib <- zioWithTime.fork                                            //interrupts evaluation of fiber
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interrupt /* <-- is an effect, blocks the calling fiber until the interrupted fiber is done/interrupted */
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join  // Note : Interrupted fiber .join call garda Interruption message generated
  } yield result

  val interruption_v2 = for {
    fib <- zioWithTime.fork //interrupts evaluation of fiber                               // this changed from above so interrupt call doesnt block the effect in-front "(ZIO.sleep(.......)"
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interruptFork  // even tho .join is not called since fiber is a cheap dataStructure stored in Heap low time computing "leaked fiber" collected by garbage collector
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  /*
    Automatic interruption Cases
   */
    // 1 - outliving a parent fiber
  val parentEffect =
    ZIO.succeed("spawning fiber").debugThread *>
      // zioWithTime.fork *> // child fiber
      zioWithTime.forkDaemon *> // this fiber will now be a child of the MAIN fiber
      ZIO.sleep(1.second) *>
      ZIO.succeed("parent successful").debugThread

  val testOutlivingParent = for {
    parentEffectFib <- parentEffect.fork
    _ <- ZIO.sleep(3.seconds)
    _ <- parentEffectFib.join
  } yield ()
   // child fibers will be (automatically) interrupted if the parent fiber is completed
                                        // ->    UNLESS you use "forkDaemon" making the fiber a child of the main program

  // 2 - racing
  val slowEffect = (ZIO.sleep(2.seconds) *> ZIO.succeed("slow").debugThread). onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)
  val fastEffect = (ZIO.sleep(1.seconds) *> ZIO.succeed("fast").debugThread). onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)
  val aRace = slowEffect.race(fastEffect) // both effect executed independently each on its own fiber
  val testRace = aRace.fork *> ZIO.sleep(3.seconds)


  /**
   * Exercise
   */
  /*
    1 - implement a timeout function
      - if zio is successful before timeout => a successful effect
      - if zio fails before timeout => a failed effect
      - if zio takes longer than timeout => interrupt the effect
   */
  def timeout[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,A] = for {
    fib <- zio.fork
    _ <- ZIO.sleep(time) *> fib.interruptFork
    result <- fib.join
  } yield result

  val testTimeout = timeout(
    ZIO.succeed("Starting....").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread,
    1.seconds
  ).debugThread

  /*
      2 - timeout v2
        - if zio is successful before timeout => a successful effect with Some(a)
        - if zio fails before timeout => a failed effect
        - if zio takes longer than timeout => interrupt the effect, return a successful effect with None
        // hint: foldCauseZIO & Cause.isInterrupted method
     */

  def timeout_v2[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =
    timeout(zio, time).foldCauseZIO(
      cause => if(cause.isInterrupted) ZIO.succeed(None) else ZIO.refailCause(cause), // ***IMP .refailCause return E from Cause[E]
      value => ZIO.succeed(Some(value))
    )

  // OWN
  def timeout_v3[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =  for {
    fib <- zio.fork
    _ <- ZIO.sleep(time) *> fib.interruptFork
    result <- fib.join.foldCauseZIO(
      cause => if(cause.isInterrupted) ZIO.succeed(None) else ZIO.refailCause(cause),
      value => ZIO.succeed(Some(value))
    )
  } yield result

  val testTimeout_v2 = timeout_v2(
    ZIO.succeed("Starting...").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread,
    1.second
  ).debugThread


  def run = interruption
}
