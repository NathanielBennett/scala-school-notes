package com.guardian.gcp.futures.alvin

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureThree extends App {


  def sleep(time: Long, threadId: Option[String] = None) = {
    println(s"Thread ${threadId.getOrElse("Default")} sleeping for ${time} millis"
    )
    Thread.sleep(time)
  }

  println(s"Starting calculation")

  val f = Future {
    sleep(Random.nextInt(1500))
    42
  }

  println("Before on complete")

  f.onComplete {
    case Success(v) => println(s" Got the callback: $v")
    case Failure(error) => println(s"Got error ${error.getStackTrace}")
  }

  println("A ..."); sleep(100, Some("A"))
  println("B ..."); sleep(100, Some("B"))
  println("C ..."); sleep(100, Some("C"))
  println("D ..."); sleep(100, Some("D"))
  println("E ..."); sleep(100, Some("E"))
  println("F ..."); sleep(100, Some("F"))
  println("G ..."); sleep(100, Some("G"))
  println("H ..."); sleep(100, Some("H"))
  sleep(2000)

}
