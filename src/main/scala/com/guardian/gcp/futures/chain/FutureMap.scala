package com.guardian.gcp.futures.chain


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object FutureMap extends App{

  def addTwo(n: Int): Future[Int] = Future{ n + 2 }
  def addTwoAndDouble(n: Int): Future[Int] = {
     addTwo(n).map( res => res * 2)
  }



  val l = addTwo(3)
  l
}