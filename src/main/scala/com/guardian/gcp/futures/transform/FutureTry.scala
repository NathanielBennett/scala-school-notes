package com.guardian.gcp.futures.transform

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object FutureTry extends App {

  def getAFutureInt(millis: Long): Future[Int] = Future {
    println(s"Sleeping for ${millis} millis")
    Thread.sleep(millis)
    40
  }

  val tfMap = getAFutureInt(-100)

    .map{ i => s"Future: $i" }
/*
    .onComplete{
      case Success(s) => println(s"On Complete: $s")
      case Failure(e) => println(s"Error ${e.getMessage}")
    }
*/
  val tfFMap = getAFutureInt(100).flatMap{ i => Future.successful(s"Future: $i")}




  val tf = getAFutureInt(750).transform{
    case Success(value) => Try(s"Future : $value")
    case Failure(exception)  => Try("exception")
  }


  val tfw  = getAFutureInt(1500).transformWith{
    case Success(s) => Future{s"Future : $s"}
    case Failure(exception) => Future{exception.getMessage}
  }

  Thread.sleep(2000)

}
