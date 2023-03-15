package com.guardian.gcp.futures

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureBlocking extends App {

  implicit val baseTime = System.currentTimeMillis()

  def sleep(theadCounter: Int): Future[Int] = Future {
    val sleepTime = Random.nextInt(500)
    println(s"Thread: $theadCounter Thread ID ${Thread.currentThread().getId} Sleeping for ${sleepTime} ")
    Thread.sleep(sleepTime)
    println(s"Thread: $theadCounter Done sleeping" )
    theadCounter
  }


  val first = Await.result(sleep(0), Duration.Inf)

  val  awaited = (1 to 5).map { c =>
    val futureInt = sleep(c)
    Await.result(futureInt, Duration.Inf)
  }.toList

  println(awaited)



}