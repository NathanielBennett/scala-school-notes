package com.guardian.gcp.futures.transform

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.{Await, Future}


object FutureMap extends App {


    def makeRandomNumber(): Future[Int] = Future {
       println("Sleeping for 250 ms")
       Thread.sleep(250)
       Random.nextInt(250)
    }

    val futureWelcome = makeRandomNumber().map(num => s"Ahoy there, Sailor number: $num")

    val s = Await.result(futureWelcome, Duration.Inf )
    println(s"*$s*")





}
