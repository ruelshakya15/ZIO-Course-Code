package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*
import zio.stm.*
import zio.stm.ZSTM.UnlessSTM

object TransactionalEffects extends ZIOAppDefault{

  // STM (data structure) like ZIOs = "atomic effects" -> once an effect start will not until complete  (cannot be interrupted)
  val anSTM: ZSTM[Any, Nothing, Int] = STM.succeed(42)
  val aFailedSTM = STM.fail("something bad")
  val anAttemptSTM: ZSTM[Any, Throwable, Int] = STM.attempt(42 / 0)
  // map, flatMap, for comprehensions like ZIOs too

  // type aliases  -> like ZIOs UIO,Task....
  val ustm: USTM[Int] = STM.succeed(2)   // ZSTM usually dont have any requirement so we normally use USTM / STM instead
  val anSTM_v2: STM[Nothing, Int] = STM.succeed(42)

  // STM vs ZIO
  // compose STMs to obtain other STMs
  // evaluation is FULLY ATOMIC
  // "commit" ->    converts STM to ZIO

  val anAtomicEffect: ZIO[Any, Throwable, Int] = anAttemptSTM.commit // evaluation of STM -> result in ZIO         (STM will block all shared resources when evaluated)

  // example
  def transferMoney(sender: Ref[Long], receiver: Ref[Long], amount: Long): ZIO[Any, String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if(senderBalance < amount) ZIO.fail("transfer failed: Insufficient funds") else ZIO.unit  // yo line same time ma execute huna sakcha so check garda both seems fine resulting in >1000 being updated
      _ <- sender.update(_ - amount)    //fibers ( protects against RC in refs) so, 1 may update & then 2 may update but not simultaneous execution altogether
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def exploitBuggyBank() = for {
    sender <- Ref.make(1000L)
    receiver <- Ref.make(0L)
    fib1 <- transferMoney(sender, receiver, 1000).fork
    fib2 <- transferMoney(sender, receiver, 1000).fork
    _ <- (fib1 zip fib2).join //*** NOTE: esari ni join garna milcha
    _ <- receiver.get.debugThread // should NEVER > 1000
  } yield ()


  def loop(effect: ZIO[Any, String, Unit], i: Int): ZIO[Any, Nothing, Unit] =
    if (i > 10000)
      ZIO.unit
    else
      effect.ignore *> loop(effect, i + 1)


  // STM implementation
  def transferMoneyTransactional(sender: TRef[Long], receiver: TRef[Long], amount: Long): STM[String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if (senderBalance < amount) STM.fail("transfer failed: Insufficient funds") else STM.unit 
      _ <- sender.update(_ - amount) 
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def cannotExploit() = for {
    sender <- TRef.make(1000L).commit // USTM[TRef[Long]]  -> (converts to ) ZIO
    receiver <- TRef.make(0L).commit
    fib1 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork  // converted to ZIO here as well using "commit"
    fib2 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
    _ <- (fib1 zip fib2).join //*** NOTE: esari ni join garna milcha  // NOTHING will be printed because one of the fibers will FAIL
    _ <- receiver.get.commit.debugThread // should NEVER > 1000
  } yield ()


  /*
    STM data structures
   */
    // atomic variable: TRef
    // same API: get, update, modify, set
  val aVariable: USTM[TRef[Int]] = TRef.make(42)

  // TArray
  val specifiedValuesTArray: USTM[TArray[Int]] = TArray.make(1,2,3)
  val iterableArray: USTM[TArray[Int]] = TArray.fromIterable(List(1,2,3,4,5)) // can convert any scala iterable to TArray
    // get/apply
  val tArrayGetElement: USTM[Int] = for {
    tArray <- iterableArray
    elem <- tArray(2)
  } yield elem
    // update
  val tArrayUpdateElem: USTM[TArray[Int]] = for {
    tArray <- iterableArray
    _ <- tArray.update(1, el => el + 10)
  } yield tArray
    // transform
  val transformedArray: USTM[TArray[Int]] = for {
    tArray <- iterableArray
    _ <- tArray.transform(_ * 10 ) // like a map, but in place
  } yield tArray
    // other include ( fold/fooldSTM, foreach)

  // TSet
    // create
  val specificValuesTSet: USTM[TSet[Int]] = TSet.make(1,2,3,4,5,1,2,3)
    // contains
  val tSetContainsElem: USTM[Boolean] = for {
    tSet <- specificValuesTSet
    res <- tSet.contains(3)
  } yield res
    // add
  val putELem: USTM[TSet[Int]] = for {
    tSet <- specificValuesTSet
    res <- tSet.put(7)
  } yield tSet
    // delete
  val deleteElem: USTM[TSet[Int]] = for {
    tSet <- specificValuesTSet
    res <- tSet.delete(1)
  } yield tSet
    // other include (union, intersect, fold .....)
    // removeIf, retainIf
    // transform, fold  + STM versions

  // TMap
  val aTMapEffect: USTM[TMap[String, Int]] = TMap.make(("Daniel", 123), ("Alice", 456), ("QE2", 999))
    // put
  val putElemTmap: USTM[TMap[String, Int]] = for {
    tMap <- aTMapEffect
    _ <- tMap.put("Master Yoda", 111)
  } yield tMap
    // get
  val getElemTMap: USTM[Option[Int]] = for {
    tMap <- aTMapEffect
    elem <- tMap.get("Daniel")
  } yield elem
    // delete, removeIf, retainIf
    // transform + STM
    // fold + STM
    // foreach
    // keys, values

  // TQueue
  val tQueueBounded: USTM[TQueue[Int]] = TQueue.bounded[Int](5) // ".unbounded" can be used to store limitless elements, there are more like(sliding....)
    // offer/offerALL (PUSH/PUT API)
  val demoOffer: USTM[TQueue[Int]] =
    for {
      tQueue <- tQueueBounded
      _ <- tQueue.offerAll(List(1,2,3,4,5,6))
    } yield tQueue
    // take/takeALL (PULL API)
  val demoTakeAll: USTM[Chunk[Int]] =  for {  // Chunk -> ZIO wrapper on JVM array   (as powerful as SCALA arraylist with wide amount of API operations)
    tQueue <- demoOffer
    elem <- tQueue.takeAll
  } yield elem
  // takeOption, peek
  // toList, toVector
  // size

  // TPriorityQueue
  val maxQueue: USTM[TPriorityQueue[Int]] = TPriorityQueue.make(3,4,1,2,5) // if you hover .make -> PriorityQueue requires "(implicit ord: Ordering[A])" according to which priority is determined

  /*
      Concurrent coordination
     */
  // TRef

  // TPromise
  // same API
  val tPromise: USTM[TPromise[String, Int]] = TPromise.make[String, Int]
    // await
  val tPromiseAwait: STM[String, Int] = for {
    p <- tPromise
    res <- p.await
  } yield res
    // succeed/fail/complete
  val demoSucceed: USTM[Unit] = for {
    p <- tPromise
    _ <- p.succeed(100)
  } yield ()

  // TSemaphore
  val tSemaphoreEffect: USTM[TSemaphore] = TSemaphore.make(10)
    // acquire + acquireN
  val semaphoreAcq: USTM[Unit] = for {
    sem <- tSemaphoreEffect
    _ <- sem.acquire
  } yield ()
    // release + releaseN
  val semaphoreRel: USTM[Unit] = for {
    sem <- tSemaphoreEffect
    _ <- sem.release
  } yield ()
    // with Permit  -> returns a ZIO
  val semWithPermit: UIO[Unit] = tSemaphoreEffect.commit.flatMap{ sem =>
    sem.withPermit{
      ZIO.succeed(42)
    }
  }

  // TReentrantLock - can acquire the same lock multiple times without deadlock
  // was created to solve (readers - writers problem)
  // has two locks: read lock (lower priority) and write lock (higher priority)
  val reentrantLockEffect = TReentrantLock.make
  val demoReentrantLock = for {
    lock <- reentrantLockEffect
    _ <- lock.acquireRead  // acquires the read lock (any writer will not be able to write)
    _ <- STM.succeed(100) // critical section, only those that acquire read lock can access
    rl <- lock.readLocked // status of the lock, whether it is read-locked, true in this case
    wl <- lock.writeLocked // same for writer
  } yield ()

  //  ***IMP NOTE: In a read-write lock, multiple readers are allowed to acquire the lock simultaneously, as long as no writer has acquired the write lock.
  def demoReadersWriters(): UIO[Unit] = {
    def read(i: Int, lock: TReentrantLock): UIO[Unit] = for {
      _ <- lock.acquireRead.commit
      // critical region start
      _ <- ZIO.succeed(s"[task ${i} taken the read lock, reading ...]").debugThread
      time <- Random.nextIntBounded(1000)
      _ <- ZIO.sleep(time.millis)
      res <- Random.nextIntBounded(100) // actual computation
      _ <- ZIO.succeed(s"[task $i] read value: $res").debugThread
      // critical region end
      _ <- lock.releaseRead.commit
    } yield ()

    def write(lock: TReentrantLock): UIO[Unit] = for {
      // writer
      _ <- ZIO.sleep(200.millis)
      _ <- ZIO.succeed("[writer] trying to write ...").debugThread
      _ <- lock.acquireWrite.commit
      // critical region
      _ <- ZIO.succeed("[write] i'm able to write!").debugThread
      // critical region end
      _ <- lock.releaseWrite.commit
    } yield ()

    for {
      lock <- TReentrantLock.make.commit
      fib <- ZIO.collectAllParDiscard((1 to 10).map(read(_, lock))).fork
      writersFib <- write(lock).fork
      _ <- fib.join
    } yield ()
  }

  def run = demoReadersWriters()
    // loop(cannotExploit(), 1)

}
