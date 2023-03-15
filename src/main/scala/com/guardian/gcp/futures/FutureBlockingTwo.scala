package com.guardian.gcp.futures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object FutureBlockingTwo extends App {

  implicit val baseTime = System.currentTimeMillis()

  def sleep(theadCounter: Int): Future[Int] = Future {
    val sleepTime = Random.nextInt(500)
    println(s"Thread: $theadCounter Thread ID ${Thread.currentThread().getId} Sleeping for ${sleepTime} ")
    Thread.sleep(sleepTime)
    println(s"Thread: $theadCounter Done sleeping" )
    theadCounter
  }


  val first = Await.result(sleep(0), Duration.Inf)

  val nonAwaited = (1 to 5).map { c => sleep(c) }.toList
  val a = Await.result(Future.sequence(nonAwaited), Duration.Inf)
  println(a)
}