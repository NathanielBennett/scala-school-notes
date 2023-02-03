package com.guardian.gcp.futures.alvin

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Sleeper {
  protected def sleep(i: Int) = Thread.sleep(i)
}

object Cloud extends Sleeper {


  def runAlgorithm(i: Int): Future[Int] = Future {
      val sleepTime = Random.nextInt(500)
      println(s"Sleeping ${sleepTime} before processingL $i")
      sleep(sleepTime)
      val res = i + 10
      println(s"Returning result from cloud ${res}")
      res
  }
}

object FutureFive extends App with Sleeper {


  println("Starting futures")

  val resultOne = Cloud.runAlgorithm(10)
  val resultTwo = Cloud.runAlgorithm(20)
  val resultThree = Cloud.runAlgorithm(30)

  println("Before For Comprehension")

  val result = for {
      r1 <- resultOne
      r2 <- resultTwo
      r3 <- resultThree
  } yield (r1 + r2 + r3)

  println("Before On Complete")
  result.onComplete{
    case Success(total) => println(s"Got total: ${total}")
    case Failure(error) => println(s"There was an error:"); error.printStackTrace()
  }

  println(
    "JVM Beauty sleep"
  )

  sleep(2000)
}
