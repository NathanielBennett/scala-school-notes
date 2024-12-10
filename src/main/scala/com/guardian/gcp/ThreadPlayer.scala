package com.guardian.gcp

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

object ThreadPlayer extends App {


  implicit val ec = ExecutionContext.global

  val t = (for {
    segment <- (1 to 3).toList
    transaction <- (1 to 3).toList
  } yield ( segment, transaction ) )
    .groupBy{ case(s, _) => s }


  val results = {
    t.foldLeft(Future(List[(String, Int)]())) { case ( futureSegmentTotals, (segment, transactions) ) =>
      val segmentTotal = transactions.foldLeft(Future[Int](0)) { case (total, transaction) => Future {
        val sleep = Random.nextInt(100)
        println(s"Sleeping $sleep for $segment, transation: $transaction")
        Thread.sleep(sleep)
        println(s"dONE Sleeping $sleep for $segment, transation: $transaction")
        total.map(t => t + 1)
      }.flatten
    }

    for {
      total <- segmentTotal
      segmentTotals <- futureSegmentTotals
    } yield (s"Segment $segment", total) :: segmentTotals
    }
  }

  Await.result(results, Duration.Inf).map { case(segment, total) => println(s"Segment: $segment, Total: $total") }
}
