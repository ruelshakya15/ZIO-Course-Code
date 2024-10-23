package com.rockthejvm.part2effects

import zio._

object ZIOApps {

  val meaningOfLife: UIO[Int] = ZIO.succeed(42)


  // WAYS TO RUN ZIOs
  // 1)
  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    given trace: Trace = Trace.empty
    Unsafe.unsafeCompat{ unsafe =>
      given u: Unsafe = unsafe

      println(runtime.unsafe.run(meaningOfLife))
    }
  }
}

// 2)
object BetterApp extends ZIOAppDefault {
  // provides runtime, trace, .. by itself
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIOApps.meaningOfLife.debug
}

// 3)
// not needed
object ManualApp extends ZIOApp{

  implicit def environmentTag: zio.EnvironmentTag[ManualApp.type] = ???

  type Environment = this.type

  def bootstrap: ZLayer[ZIOAppArgs with Scope, Any, ManualApp.type] = ???

  def run: ZIO[ManualApp.type with ZIOAppArgs with Scope, Any, Any] = ???
}