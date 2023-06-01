package com.guardian.gcp.futures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}

class Connection {
  def getCurrentValue(currency: String): Double =  currency match {
    case "USD" => Random.between(1.10, 1.75)
    case _ => 1.4
  }
  def buy(amount: Int, quote: Double): Double = amount * quote
}


object FutureComprehensionOne extends App {

  val amount = 100
  val connection = new Connection()


  def isProfitable(quote: Double): Boolean = quote > 75

  val rateQuote = Future {
    connection.getCurrentValue("USD")
  }
  println("Hello")

  for (quote <- rateQuote) {
    val purchase = Future {
      if (isProfitable(quote)) connection.buy(amount, quote)
      else throw new Exception("Not profitable")
    }
    println("Hello 2")

    for (amount <- purchase) {
      println(s"Purchased: $amount  USD for $purchase  ")
    }

    Thread.sleep(2000)
    println("Done")
  }


}




