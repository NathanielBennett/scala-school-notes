package com.guardian.gcp.futures.baeldung

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


object FutureSuccessOne extends App {

  def magicNumber(sleep: Int = 300): Int = {
     println(s"Sleeping $sleep")
     Thread.sleep(sleep)
     23
  }

  def multiply(multiplier: Int): Future[Int] = {
    println(s"Multiplyting by $multiplier")
    if (multiplier == 0) {
      Future.successful(0)
    }
    else {
      Future(multiplier * magicNumber())
    }
  }

  multiply(4).onComplete{
    case Success(result) => println(s"Result is $result")
    case Failure(e) => println(s"Error: ${e.getStackTrace}")
  }
  1
  multiply(0).onComplete{
    case Success(result) => println(s"Result is $result")
    case Failure(e) => println(s"Error: ${e.getStackTrace}")
  }

  Thread.sleep(2000)
}
