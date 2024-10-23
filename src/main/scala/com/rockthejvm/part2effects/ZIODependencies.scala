package com.rockthejvm.part2effects

import zio.*

import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {

  // *** NOTE: FIRST PART where Service classes Defined are in "ServiceModel.scala"
  // Service definitions moved to ServiceModel.scala
  // reason: ZLayer.make and ZIO.provide use macros which mistakenly require the services to be in another source file

  import ServiceModel.*

  val subscriptionService = ZIO.succeed( // THIS PATTERN => "DEPENDENCY INJECTION"(DI)
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*  THE ABOVE IS A "CLEAN DI" but ;
      drawbacks
      - does not scale for many services
      - DI can be 100x worse
        - pass dependencies partially
        - not having all deps in the same place         [TO UNDERSTAND BETTER WATCH VIDEO]
        - passing dependencies multiple times
     */

  // eg: of passing dependencies multiple times DRAWBACK
  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call
    - <- sub.subscribeUser(user)
  } yield ()

  //   ^^^^^ FOR ABOVE
  // risk leaking resources if you subscribe multiple users in the same program( because "subscriptionService" instantiated every time new User passed & if program flow is not appropriate)
  val program = for {
    _ <- subscribe(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe(User("Daniel", "daniel@rockthejvm.com"))
  } yield ()

  // SOLUTION : alternative for above :- [Using ZIO "requirement"]
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe_v2(User("Daniel", "daniel@rockthejvm.com"))
  } yield ()

  /*  AD for above implementation of DI[Using ZIO "requirement"] :-
   - we don't need to care about dependencies until the end of the world
   - all ZIOs requiring this dependency will use the same instance
   - can use different instances of the same type for different needs (e.g. testing)
   - layers can be created and composed much like regular ZIOs + rich API
  */

  /**
   * ZLayers
   * they can be built just like ZIOs -> .succeed, .fail, .attempt ....
   */
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] =
    ZLayer.succeed(ConnectionPool.create(10))
  // a layer that requires a dependency (higher layer) can be built with ZLayer.fromFunction
  // (and automatically fetch the function arguments and place them into the ZLayer's dependency/environment type argument)
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create _)  // THIS functionality in done BTS using macros
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] =
    ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create _)

  // composing layers(COMBINING LAYERS)
  // vertical composition(>>>) (rChannel of connectionPool) & (vChannel of DBLayer)
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer
  // horizontal composition (++): combines the dependencies of both layers AND the values of both layers
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase & EmailService] = databaseLayerFull ++ emailServiceLayer

  // mix & match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  // BEST PRACTICE: write "factory" methods exposing layers in the companion objects of the services[ DONE in "ServiceModel.scala" ]
  val runnableProgram = program_v2.provideLayer(userSubscriptionLayer)

  // MAGIC (ZIO auto builds/composes layers) by traversing graph through macros
  val runnableProgram_v2 = program_v2.provide(
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,              // if you miss a dependency layer at compile time ZIO will inform you
    ConnectionPool.live(10),
    // ZIO will tell you: // if you're missing a layer
                         // and if you have multiple layers of the same type      eg: UserSubscription.live 2 times
                         // and tell you the dependency graph!
                         // ZLayer.Debug.tree,
    ZLayer.Debug.mermaid,
  )

  // MAGIC_V2
  val userSubscriptionLayer_v2: ZLayer[Any, Nothing, UserSubscription] = ZLayer.make[UserSubscription](
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,
    ConnectionPool.live(10)
  )

  // passthrough                                      // return rChannel dependency in vChannel as well
  val dbWithPoolLayer:ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase] = UserDatabase.live.passthrough
  // service = take a dep and expose it as a value to further layers
  val dbService = ZLayer.service[UserDatabase]
  // launch = creates a ZIO that uses the services and never finishes   eg (for cases like a live server running continuously, game loops,etx
  val subscriptionLaunch: ZIO[EmailService with UserDatabase, Nothing, Nothing] = UserSubscription.live.launch
  // memoization - done by default all Layers that req same dep same instance is used again once instantiated unless we do , ->  "UserDatabase.live.fresh,"

    /*
      Already provided services: Clock, Random, System, Console
     */

    // these return "ZIOs"
    val getTime = Clock.currentTime(TimeUnit.SECONDS)
    val randomValue = Random.nextInt
    val sysVariable = System.env("HADOOP_HOME")
    val printlnEffect = Console.printLine("This is ZIO")

  def run = runnableProgram_v2.debug



}
