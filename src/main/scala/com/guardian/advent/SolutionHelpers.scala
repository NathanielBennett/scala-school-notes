package com.guardian.advent

import com.guardian.advent.grid.GridEntry

import scala.collection.AbstractSeq
import scala.collection.immutable.LinearSeq

trait SolutionHelpers {

  implicit class RichBoolean(boolean: Boolean) {
      final def toOption[A](a: => A): Option[A] = if(boolean) Some(a) else None
  }

  implicit class RichString(string: String) {
     final def toStringList(separator: Char): List[String] = string.split(separator).toList
  }

  implicit class RichIterator[A](iterator: Iterator[A]) {
    def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): Map[K, B] =
      iterator.to(LazyList).groupMapReduce(key)(f)(reduce)

  }

  implicit def gridEntryListToString[A](gridEntries: Seq[GridEntry[A]]): String = {
     gridEntries.foldLeft(new StringBuilder()) { case (strinBuilder,a) => strinBuilder.append(a.value)  }.toString
  }


  implicit class RichTuple[A, B](tuple: (A, B)) {

    def left = tuple._1
    def right = tuple._2
    lazy val key = s"$left-$right"
    def toValueAndKey: (String, (A, B)) = {
      (key, tuple)
    }
    def reverse: (B, A) = (right, left)

    def equalsLeft(other: (A, B)): Boolean = other.left == left
    def equalsRight(other: (A, B)): Boolean = other.right == right
  }

  implicit class RichLinearSeq[A, B <: LinearSeq[A]](linearSeq: LinearSeq[A]) {
    def tailHead: A = linearSeq.tail.head
    def doesNotContain(a: A): Boolean =  !linearSeq.contains(a)
    def tailEmpty: Boolean = linearSeq.size == 1
    def uniquePermutations(toB: IndexedSeq[A] => B): Iterator[B] = {
      val is = (0 to linearSeq.length).flatMap{ _ => linearSeq}
      toB(is).combinations(linearSeq.length).asInstanceOf[Iterator[B]]
    }
  }

   implicit class RichTupleSequence[A,B](tuples: LinearSeq[(A, B)]) {

    type T = (A, B)
    type U = (B, A)
 /*   implicit def toL(linearSeq: LinearSeq[T]): L = linearSeq.asInstanceOf[L]
    implicit def toM(linearSeq: LinearSeq[U]): M = linearSeq.asInstanceOf[M]
*/
    def tupleListToMap : Map[String, T] = tupleMap[String](t => t.key)
    def tupleMapLeft: Map[A, T] = tupleMap{ t => t.left }
    def tupleListRight: Map[B, T] = tupleMap{ t => t.right }

    def invert = tuples.map{ t => t.reverse }
    def leftMap = listMap[A]{ case t => t.left}
    def rightMap= listMap[B] {case t => t.right}

    def leftList = tuples.map { case t => t.left }
    def rightList = tuples.map { case t => t.right }
    def listMap[K](makeKey: T => K): Map[K, LinearSeq[T]] =
      tuples.groupMap { t => makeKey(t)}{ case(a, b) => (  a, b ) }

    def listMapLeft = mapToTupleValue[A, B]( t => t.left, t => t.right )
    def listMapRight = mapToTupleValue[B, A]( t => t.right, t => t.left )
    private def mapToTupleValue[K, D](makeKey: T => K, makeValue: T => D) : Map[K, LinearSeq[D]] =
      tuples.groupMap{ case t: T => makeKey(t) } { case t: T => makeValue(t) }

    private def tuple[K](key: K, tuple: (A,B)) = (key, tuple)
    private  def tupleMap[K](makeKey: ((A, B)) => K): Map[K, (A,B)] =
      tuples.map { t =>
        val key = makeKey(t.left, t.right)
        tuple(key, t)
      }.toMap
  }

  implicit class RichIntTupleSequence(tuples: LinearSeq[(Int, Int)]) extends RichTupleSequence[Int, Int](tuples) {
    def distinctInts: List[Int] = (tuples.leftList ++ tuples.rightList).toSet.toList
  }

  implicit class RichTupleIndexedSequence[A](tuples: LinearSeq[(A, Int)]) extends RichTupleSequence[A, Int](tuples) {
    def isOrdered: Boolean = {
        def loop(remaining: LinearSeq[(A, Int)], maybeLastIndex: Option[Int] = None): Boolean = {
            if (remaining.isEmpty) true
            else {
              val currentIndex = remaining.head.right
              val inOrder = maybeLastIndex.map { lastIndex => currentIndex > lastIndex }.getOrElse(true)
              inOrder && loop(remaining.tail, Some(currentIndex))
            }
        }
        loop(tuples)
    }
    def order: LinearSeq[(A, Int)] = tuples.sortBy{ t => t.right}
  }

  implicit class RichList[A](list: List[A]) {
    def toTuple: Option[(A, A)] = list match {
      case List(a, b) => Some((a, b))
      case _ => None
    }

    def toTuples: List[(A, A)] = {
      def loop(current: List[A], acc: List[(A, A)] = List.empty ): List[(A, A)] = {
        current match {
          case Nil => acc.reverse
          case head :: Nil => acc.reverse
          case head :: next :: tail => loop(tail, (head, next) :: acc)
        }
      }
      loop(list)
    }

    def dropRightWhile(filter: A => Boolean): List[A] = {
      list.reverse.dropWhile(filter).reverse
    }

    def sliceTo(from: Int, to: Int) = list.slice(from, to + 1)
  }

  implicit class RichTupleList[A](list: List[(A, A)]) {
    def flattenList: List[A] = {
      def loop(tuples: List[(A, A)], acc: List[A] = List.empty): List[A] = {
         tuples match {
           case Nil => acc.reverse
           case head :: tail =>
             val (left, right) = head
             loop(tail, List(right, left) ::: acc)
         }
      }
      loop(list)
    }
  }
}


