package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*
import sun.misc

import scala.collection.immutable.Queue

abstract class Mutex{
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}

object Mutex {
  type Signal = Promise[Nothing, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  val unlocked = State(locked = false, Queue())
  given trace: Trace = Trace.empty

  def make: UIO[Mutex] = Ref.make(unlocked) /*UIO[Ref[Stat]]*/.map(createInterruptibleMutex) // <- call 2 methods below here to check

  def createInterruptibleMutex(state: Ref[State]) =
    new Mutex {
      /* [ For "acquire" ]
       Change the state of the Ref
       - if the mutex is unlocked, lock it
       - if the mutex is locked, state becomes (true, queue + new signal) and WAIT on that signal
      */
      override def acquire = ZIO.uninterruptibleMask { restore =>
        Promise.make[Nothing, Unit].flatMap { signal =>

          val cleanup: UIO[Unit] = state.modify {// Need to be careful for cleanUp as .onInterrupt because there time diff between being blocked by signal.await and moment task is interrupted[SEE VIDEO,12:00]
            case State(flag, waiting) =>                                                                                    // there may be other fiber blocked and queued in signal as well
            val newWaiting = waiting.filterNot(_ eq signal)      //here eq used instead of != because we need reference                // need to find out signal in the Queue
            // blocked only if newWaiting != waiting => release the mutex
            val wasBlocked = newWaiting != waiting
            val decision = if (wasBlocked) ZIO.unit else release

            decision -> State(flag, newWaiting)
          }.flatten

          state.modify { // ***IMP: NOTE ; -> operator creates a tuple
            case State(false, _) => ZIO.unit -> State(locked = true, Queue())
            case State(true, waiting) => restore(signal.await).onInterrupt(cleanup) -> State(locked = true, waiting.enqueue(signal))  //*** NOTE: tuple ko 1st part ma wait gare ni 2nd part already evaluated i.e Queue updated that that task's signal , and 1st part ma we wait for some other task to complete the promise
          }.flatten // because: UIO[UIO[...] returned by modify / can also change .flatMap => .map above
        }
      }

      /* [For "release" ]
        Change the state of the Ref
        - if the mutex is unlocked, leave the state unchanged
        - if the mutex is locked
          -> if the queue is empty, unlock the mutex
          -> if the queue is non-empty, take a signal out of the queue and complete it
       */
      override def release = state.modify {
        case State(false, _) => ZIO.unit -> unlocked
        case State(true, waiting) =>
          if (waiting.isEmpty) ZIO.unit -> unlocked
          else {
            val (first, rest) = waiting.dequeue // *** running PM in val def so tuples values associated with (first, last)
            first.succeed(()).unit -> State(locked = true, rest)
          }
      }.flatten // because: ZIO.unit returned instead of normal Unit


    }

  // PROBLEM: in cases of interruptions: causes deadlock as interrupted task doesn't release Signal
  def createSimpleMutex(state: Ref[State]) = new Mutex {
    /* [ For "acquire" ]
     Change the state of the Ref
     - if the mutex is unlocked, lock it
     - if the mutex is locked, state becomes (true, queue + new signal) and WAIT on that signal
    */
    override def acquire = Promise.make[Nothing, Unit].flatMap { signal =>
      state.modify {                   // ***IMP: NOTE ; -> operator creates a tuple
        case State(false, _) => ZIO.unit -> State(locked = true, Queue())
        case State(true, waiting) => signal.await -> State(locked = true, waiting.enqueue(signal))
      }.flatten                                 // because: UIO[UIO[...] returned by modify / can also change .flatMap => .map above
    }


    /* [For "release" ]
      Change the state of the Ref
      - if the mutex is unlocked, leave the state unchanged
      - if the mutex is locked
        -> if the queue is empty, unlock the mutex
        -> if the queue is non-empty, take a signal out of the queue and complete it
     */
    override def release = state.modify {
      case State(false, _) => ZIO.unit -> unlocked
      case State(true, waiting) =>
        if (waiting.isEmpty) ZIO.unit -> unlocked
        else {
          val (first, rest) = waiting.dequeue  // *** running PM in val def so tuples values associated with (first, last)
          first.succeed(()).unit -> State(locked = true, rest)
        }
    }.flatten   // because: ZIO.unit returned instead of normal Unit


  }

  // OWN (TODO: doesnt work as expected you can try to fix it if you have time)
  def make_v2: UIO[Mutex] = Ref.make(unlocked)/*UIO[Ref[Stat]]*/.map { (state: Ref[State]) =>
    new Mutex {
      /* [ For "acquire" ]
       Change the state of the Ref
       - if the mutex is unlocked, lock it
       - if the mutex is locked, state becomes (true, queue + new signal) and WAIT on that signal
      */
      override def acquire: UIO[Unit] = for {
        promise <- Promise.make[Nothing, Unit]
        stateValue <- state.get
        _ <- if (!stateValue._1) state.update(_ => State(locked = true, Queue()))
            else {
              state.update(s => State(locked = true, s._2.enqueue(promise)))
              promise.await
            }
      } yield ()


      /* [For "release" ]
        Change the state of the Ref
        - if the mutex is unlocked, leave the state unchanged
        - if the mutex is locked
          -> if the queue is empty, unlock the mutex
          -> if the queue is non-empty, take a signal out of the queue and complete it
       */
      override def release: UIO[Unit] = for {
        stateValue <- state.get
        _ <- if (stateValue._1) {
          stateValue._2.isEmpty match
            case _: true => state.update(_ => State(locked = false, Queue()))
            case _: false =>
              val promiseZIO = state.modify(v => {
                val deQueueVal = v._2.dequeue
                (deQueueVal._1, State(false, deQueueVal._2))
              })
              promiseZIO.flatMap(_.succeed(()))
        } else {
          ZIO.unit
        }
      } yield ()


    }
  }
}


object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks() =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[task ${i}] working....").debugThread
        result<- workInCriticalRegion()
        _ <- ZIO.succeed(s"[task ${i} got result: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _ <- ZIO.succeed(s"[task $id] waiting for mutex").debugThread
      _ <- mutex.acquire
      // critical region start....................
      _ <- ZIO.succeed(s"[task ${id}] mutex acquired, working ....").debugThread
      result <- workInCriticalRegion().onInterrupt(mutex.release) // release mutex as odd task in critical region(after completing acquire phase & was not interrupted in that time) would block other tasks resulting in deadlock
      _ <- ZIO.succeed(s"[task ${id}] got result: $result, releasing mutex").debugThread
      // critical region end.......................
      _ <- mutex.release
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"[task $id] was interrupted.").debugThread)
      //.onError(cause => ZIO.succeed(s"[task $id] ended in error: $cause").debugThread)    //***NOTE: commented this because unlike video Errors caught when running
  }

  def demoLockingTasks = for {
    mutex <- Mutex.make
    tasks <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0)
      createTask(id, mutex)
    else for {
      fib <- createTask(id, mutex).fork
      _ <- ZIO.sleep(2500.millis) *> ZIO.succeed(s"interrupting task $id").debugThread *> fib.interrupt
      result <- fib.join
    } yield result

  /*
      _ _ X _ _ _ _ _ _ _     (if odd no task gets mutex and is interrupted , doesnt release mutex thus blocking whole operation)
      2.5s => all the odd tasks will be interrupted
     */
  def demoInterruptingTasks() = for {
    mutex <- Mutex.make
    fib1 <- createInterruptingTask(1, mutex).fork
    fib2 <- createInterruptingTask(2, mutex).fork
    fib3 <- createInterruptingTask(3, mutex).fork
    fib4 <- createInterruptingTask(4, mutex).fork
    fib5 <- createInterruptingTask(5, mutex).fork
    fib6 <- createInterruptingTask(6, mutex).fork
    fib7 <- createInterruptingTask(7, mutex).fork
    fib8 <- createInterruptingTask(8, mutex).fork
    fib9 <- createInterruptingTask(9, mutex).fork
    fib10 <- createInterruptingTask(10, mutex).fork
    _ <- fib1.await
    _ <- fib2.await
    _ <- fib3.await       // using .await instead of .join because : -  (.join case) => fiber fails or is interrupted, join will fail with the same error or interruption.,
    _ <- fib4.await                                       // -  (.await case) returns an Exit value, which encapsulates the result of fiber’s execution, whether it was (success, fail, or was interrupted)
    _ <- fib5.await
    _ <- fib6.await
    _ <- fib7.await
    _ <- fib8.await
    _ <- fib9.await
    _ <- fib10.await
  } yield ()

  def run = demoInterruptingTasks()

        //    ZIO.succeed(println /* ***IMP: Code generation */(
        //    s"""
        //       |${(1 to 10).map(i => s"fib${i} <- createInterruptingTask($i, mutex).fork").mkString("\n")}
        //       |${(1 to 10).map(i => s"_ <- fib${i}.await").mkString("\n")}
        //       |""".stripMargin
        //  ))
}
