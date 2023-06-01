package com.guardian.gcp.futures.transform

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random


object FutureFlatMapAsMap extends App {


    def makeRandomNumber(): Future[Int] = Future {
       println("Sleeping for 250 ms")
       Thread.sleep(250)
       Random.nextInt(250)
    }

   def makeNumericWelcome(prisonerNumber: Int): Future[String] = Future {
     val sleep = Random.nextInt(250)
     println(s"Sleeping: $sleep ms")
     Thread.sleep(sleep)
     if (prisonerNumber % 2 == 0) s"Ahoy there, Sailor number: $prisonerNumber"
       else s"You ain't from round here are you $prisonerNumber"
     }

     val futureWelcome = makeRandomNumber.map(number => makeNumericWelcome(number)).flatten

     val s = Await.result(futureWelcome, Duration.Inf )
     println(s"*$s*")

}
