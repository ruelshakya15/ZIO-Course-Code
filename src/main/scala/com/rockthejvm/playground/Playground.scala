package com.rockthejvm.playground

import zio.*
import com.rockthejvm.utils.*

object Playground extends ZIOAppDefault {
  val evens: ZIO[Any, List[String], List[Int]] =
    ZIO.validate(List(1, 2, 3, 4, 5)) { n =>
      if (n % 2 == 0)
        ZIO.succeed(n)
      else
        ZIO.fail(s"$n is not even")
    }

  val r1: ZIO[Any, List[String], List[Int]] = evens.mapError(_.reverse)
  val r2: ZIO[Any, List[String], List[Int]] = evens.flip.map(_.reverse).flip
  val r3: ZIO[Any, List[String], List[Int]] = evens.flipWith(_.map(_.reverse))

  val x = ZIO.succeed(None)
  val y = x.someOrFail(new RuntimeException("Hello")).mapError(_.getMessage)

  def x(n: Int): Task[String] = if (n == 1) ZIO.succeed("one") else ZIO.fail(new RuntimeException("you failed"))

  val result: ZIO[Any, String, Throwable] = x(2).flip.debugThread
  def run =  y
  //result// flipping here doesnt cause a Stack trace and 2 is printed normally
}