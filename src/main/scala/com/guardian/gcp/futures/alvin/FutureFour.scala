package com.guardian.gcp.futures.alvin

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureFour extends App {


  def sleep(i: Long) = Thread.sleep(i)

  def someLongRunningShizzle(i: Long) = Future {
    println(s"sleeping in the future: $i")
    i + 1
  }

  someLongRunningShizzle(Random.nextInt(1000)).onComplete {
    case Success(result) => println(s"result = $result")
    case Failure(e) => e.printStackTrace
  }

  sleep(2000)
}
