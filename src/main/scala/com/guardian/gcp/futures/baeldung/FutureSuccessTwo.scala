package com.guardian.gcp.futures.baeldung

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


object FutureSuccessTwo extends App {

  def magicNumber(sleep: Int = 300): Int = {
     println(s"Sleeping $sleep")
     Thread.sleep(sleep)
     23
  }

  def multiply(multiplier: Int): Future[Int] = Future.fromTry( Try {
     println(s"Multiplying by $multiplier")
     magicNumber() * multiplier
  })

  multiply(7).onComplete{
    case Success(result) => println(s"Result is $result")
    case Failure(e) => println(s"Error: ${e.getStackTrace}")
  }


  multiply(0).onComplete{
    case Success(result) => println(s"Result is $result")
    case Failure(e) => println(s"Error: ${e.getStackTrace}")
  }

  Thread.sleep(2000)
}
