package com.guardian.gcp.futures.alvin

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FutureOne extends App {


  implicit val baseTime = System.currentTimeMillis

  def sleep(time: Long) = Thread.sleep(time)

  val f = Future {
    sleep(500)
    1 + 1
  }

  println(
    "done"
  )
  val res = Await.result(f, 1.second)
  println(s"Res $res")

}
