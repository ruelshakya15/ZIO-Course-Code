package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault {

  def blockingTask(n: Int) : UIO[Unit] =
    ZIO.succeed(s"running blocking task $n").debugThread *>
      ZIO.succeed(Thread.sleep(10000)) *>
      blockingTask(n)

  // thread starvation
  val program = ZIO.foreachPar((1 to 100).toList)(blockingTask)


  // if we know an operation has blocking effect we can convert a blocking operation into a ZIO effect using "attemptBlocking"
  // blocking code is run on a dedicated thread pool for blocking operations, preventing it from starving the main thread pool
  // BLOCKING THREAD POOL
  val aBlockingZIO = ZIO.attemptBlocking{ // ".attemptBlocking" makes ZIO run on a separate threadPool
    println(s"[${Thread.currentThread().getName}] running a long computation ....")
    Thread.sleep(10000)
    42
  }

  // blocking code cannot (usually be interrupted
  val tryInterrupting = for {
    blockingFib <- aBlockingZIO.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting...").debugThread *> blockingFib.interrupt  // even after interruption program keeps running
    mol <- blockingFib.join
  } yield mol

  // we can use "attemptBlockingInterrupt" to block a blocking threadpool
  // based on Thread.interrupt (in java) -> InterruptedException (ZIO uses that bts)
  val aBlockingInterruptibleZIO = ZIO.attemptBlockingInterrupt{
    println(s"[${Thread.currentThread().getName}] running a long computation ....")
    Thread.sleep(10000)
    42
  }

  // best way to Interrupt a blocking threadPool
  // set a flag/switch
  def interruptibleBlockingEffect(canceledFlag: AtomicBoolean): Task[Unit] =
    ZIO.attemptBlockingCancelable{ // 1st part (effect)             2 parts for ".attemptBlockingCancelable"
      (1 to 100000).foreach{ element =>
        if(!canceledFlag.get()) {
          println(element)
          Thread.sleep(100)
        }

      }
    } (ZIO.succeed(canceledFlag.set(true))) // 2nd part (cancelling/interrupting effect)

  val interruptibleBlockingDemo = for {
    fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
    _ <- ZIO.sleep(2.seconds) *> ZIO.succeed("interrupting...").debugThread *> fib.interrupt    // notice in this case compared to line 26(tryInterrupting) program stops
    _ <- fib.join
  } yield ()


  // SEMANTIC blocking - no blocking of threads, done by ->  descheduling the effect/fiber
  val sleeping = ZIO.sleep(1.second) // SEMANTICALLY blocking, interruptible                  (because ZIO.sleep is just a dataStructure that tell ZIO runtime to sleep)
  val sleepingThread = ZIO.succeed(Thread.sleep(1000)) // blocking, uninterruptible
  // yield
  val chainedZIO = (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> _.debugThread) // all executed on same thread "ZScheduler-Worker-4"
  val yieldingDemo = (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> ZIO.yieldNow *> _.debugThread) // "ZIO.yieldNow" hints ZIO runtime to changeThreads

  def run = yieldingDemo

}
