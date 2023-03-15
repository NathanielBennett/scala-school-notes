package com.guardian.gcp.futures.alvin

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureTwo extends App {

  implicit val baseTime = System.currentTimeMillis

  def sleep(time: Long) = Thread.sleep(time)

  val f = Future {
    sleep(500)
    1 + 1
  }

  val f2 = Future {
    sleep(Random.nextInt(250))
    42
  }

  f2.onComplete {
    case Success(value) => println(
      s"Callback: ${value}"
    )
    case Failure(exception) => println(s"Error => ${exception.getStackTrace}")
  }

  //Take away to watch it bail on jvmm compolete
  val res = Await.result(f, 1.second)
  println(s"Res $res")


}
