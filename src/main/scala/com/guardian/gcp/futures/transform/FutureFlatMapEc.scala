package com.guardian.gcp.futures.transform

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random


object FutureFlatMapEc extends App {

    import scala.concurrent.ExecutionContext.Implicits.global

    def makeRandomNumber(): Future[Int] = Future {
       println("Sleeping for 250 ms")
       Thread.sleep(250)
       Random.nextInt(250)
    }

     val futureWelcome = makeRandomNumber().flatMap(num => WelcomeMaker.makeNumericWelcome(num))

     val s = Await.result(futureWelcome, Duration.Inf )
     println(s"*$s*")

}

object WelcomeMaker {

  def makeNumericWelcome(prisonerNumber: Int)(implicit executionContext: ExecutionContext): Future[String] = Future {
    val sleep = Random.nextInt(250)
    println(s"WelcomeMakerObject Sleeping: $sleep ms")
    Thread.sleep(sleep)
    if (prisonerNumber % 2 == 0) s"Ahoy there, Sailor number: $prisonerNumber"
    else s"You ain't from round here are you $prisonerNumber"
  }


}