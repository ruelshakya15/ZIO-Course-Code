package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault {

  // refs are purely functional atomic references        (state can be safely read and modified by multiple threads without the need for explicit synchronization.)
  val atomicMOL: ZIO[Any, Nothing, Ref[Int]] = Ref.make(42)

  // obtain a value
  val mol = atomicMOL.flatMap { ref =>
    ref.get // return a UIO[Int], thread safe getter
  }

  val setMol = atomicMOL.flatMap { ref =>
    ref.set(100) // UIO[Unit], thread-safe setter
  }

  // get + change in ONE atomic operation
  val gsMol = atomicMOL.flatMap { ref =>
    ref.getAndSet(500)
  }

  // update - run a function on the value
  val updatedMol = atomicMOL.flatMap { ref =>
    ref.update(_ * 100) // same as ref.set(f(ref.get))
  }

  // update + get in ONE operation
  val updatedMolWithValue = atomicMOL.flatMap { ref =>
    ref.updateAndGet(_ * 100) // returns the NEW value
    ref.getAndUpdate(_ * 100) // returns the OLD value
  }

  // modify
  val modifiedMol: UIO[String] = atomicMOL.flatMap { ref =>
    ref.modify(value => (s"my current value is $value", value * 100))
  } // ^^ this value is returned ,        ^^ this values stored in ref

  // example: distributing work
  def demoConcurrentWorkImpure(): UIO[Unit] = {
    var count = 0

    def task(workload: String): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"Counting words for: $workload : $wordCount").debugThread
        newCount <- ZIO.succeed(count + wordCount)
        _ <- ZIO.succeed(s"new total: $newCount").debugThread
        _ <- ZIO.succeed(count += wordCount) // updating the variable
      } yield ()
    }

    val effects = List("I love ZIO", "This Ref thing is cool", "Daniel writes a LOT of code!").map(task)
    ZIO.collectAllParDiscard(effects)
  }
  /*
    - NOT THREAD SAFE!
    - hard to debug in case of failure
    - mixing pure and impure code
   */

  // Pure Version using Refs:
  def demoConcurrentWorkPure(): UIO[Unit] = {
    def task(workload: String, total: Ref[Int]): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"Counting words for: $workload : $wordCount").debugThread
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- ZIO.succeed(s"new total: $newCount").debugThread
      } yield ()
    }

    for {
      counter <- Ref.make(0)
      _ <- ZIO.collectAllParDiscard(
        List("I love ZIO", "This Ref thing is cool", "Daniel writes a LOT of code!")
          .map(load => task(load, counter))
      )
    } yield ()
  }

  /**
   * Exercises
   */
  // 1 - refactor this code using a Ref
  def tickingClockImpure(): UIO[Unit] = {
    var ticks = 0L

    // print the current time every 1s + increase a counter ("ticks")
    def tickingClock: UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ZIO.succeed(ticks += 1)
      _ <- tickingClock
    } yield ()

    // print the total count every 5s
    def printTicks: UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      _ <- ZIO.succeed(s"TICKS: $ticks").debugThread
      _ <- printTicks
    } yield ()

    (tickingClock zipPar printTicks).unit
  }

  def tickingClockPure(): UIO[Unit] = {
    // print the current time every 1s + increase a counter ("ticks")
    def tickingClock(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    // print the total ticks count every 5s
    def printTicks(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      t <- ticks.get
      _ <- ZIO.succeed(s"TICKS: $t").debugThread
      _ <- printTicks(ticks)
    } yield ()

    for {
      ticks <- Ref.make(0L)
      _ <- tickingClock(ticks) zipPar printTicks(ticks)
    } yield ()
  }


  // 2
  def tickingClockPure_v2(): UIO[Unit] = {
    val ticksRef: UIO[Ref[Long]] = Ref.make(0L)
    // print the current time every 1s + increase a counter ("ticks")
    def tickingClock: UIO[Unit] = for {
      ticks <- ticksRef
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(_ + 1)
      _ <- tickingClock                                                                 // new reference to Ref variable created every time , so TICk = 0 always    (can watch video to better understand)
    } yield ()

    // print the total ticks count every 5s
    def printTicks: UIO[Unit] = for {
      ticks <- ticksRef
      _ <- ZIO.sleep(5.seconds)
      t <- ticks.get
      _ <- ZIO.succeed(s"TICKS: $t").debugThread
      _ <- printTicks
    } yield ()

    (tickingClock zipPar printTicks).unit
  }

  // update function maybe run more than once
  def demoMultipleUpdates: UIO[Unit] = {
    def task(id: Int, ref: Ref[Int]): UIO[Unit] =
      ref.modify(previous => (println(s"Task $id updating ref at $previous"), id))  // tasks will try to .modify again(runs multiple times) if blocked once ref is free

    for{
      ref <- Ref.make(0)
      - <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => task(i, ref)))
    } yield ()
  }

  def run = demoMultipleUpdates
}
