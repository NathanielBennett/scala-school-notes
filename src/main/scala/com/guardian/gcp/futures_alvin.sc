import scala.concurrent.Await
import scala.util.{Failure, Random, Success}

implicit val baseTime = System.currentTimeMillis

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
/*
import java.util.concurrent.{ExecutorService, Executors}
import cats.effect.{Contex}
*/


def sleep(time: Long) = Thread.sleep(time)

val f = Future {
  sleep(500)
  1 + 1
}

val res = Await.result(f,  1 second)

val f2 = Future {
  sleep(Random.nextInt(500))
  42
}

f2.onComplete {
  case Success(value)  => println(
    s"Callback: ${value}"
  )
  case Failure(exception) => println(s"Error => ${exception.getStackTrace}")
}






