package com.guardian.advent

import scala.collection.AbstractSeq
import scala.collection.immutable.LinearSeq

trait SolutionHelpers {

  implicit class RichBoolean(boolean: Boolean) {
      final def toOption[A](a: => A): Option[A] = if(boolean) Some(a) else None
  }

  implicit class RichString(string: String) {
     final def toStringList(separator: Char): List[String] = string.split(separator).toList
  }

  implicit class RichLinearSeq[A, B <: LinearSeq[A]](abstractSeq: LinearSeq[A]) {
    def tailHead: A = abstractSeq.tail.head

    def tailEmpty: Boolean = abstractSeq.size == 1
  }
  implicit class RichList[A](list: List[A]) {
    def doesNotContain(a: A): Boolean = !list.contains(a)
  }

  implicit def gridEntryListToString[A](gridEntries: Seq[GridEntry[A]]): String = {
     gridEntries.foldLeft(new StringBuilder()) { case (strinBuilder,a) => strinBuilder.append(a.value)  }.toString
  }

/*
  implicit class RichAbstractSeq[A](seq: AbstractSeq[A]) {

      def uniquePermutations[ B <: AbstractSeq[A]]()(toB: AbstractSeq[A] => B): B =
        (0 to seq.length).flatMap{ _ => toB(seq)}.combinations(seq.length)

  }
*/

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

  implicit class RichTupleList[A,B, L <: LinearSeq[(A, B)], M <: LinearSeq[(B, A)]](tuples: L) {

    type T = (A, B)
    type U = (B, A)
    implicit def toL(linearSeq: LinearSeq[T]: L = linearSeq.asInstanceOf[L]
    implicit def toM(linearSeq: LinearSeq[B]: M = linearSeq.asInstanceOf[M]

    

    def tupleListToMap : Map[String, T] = tupleMap[String](t => t.key)
    def tupleMapLeft: Map[A, T] = tupleMap{ t => t.left }
    def tupleListRight: Map[B, T] = tupleMap{ t => t.right }

    def invert: M = tuples.map{ t => t.reverse }.asInstanceOf[M]
    def leftMap: Map[A,L] = listMap[A]{ case t => t.left}
    def rightMap: Map[B,L] = listMap[B] {case t => t.right}
    def leftList: LinearSeq[A] = tuples.map { case t => t.left }
    def rightList: LinearSeq[B] = tuples.map { case t => t.right }


    def listMap[K](makeKey: T => K): Map[K, LinearSeq[T]] = tuples.groupMap { t => makeKey(t)}{ case(a, b) => (  a, b ) }

    def listMapLeft: Map[A, List[B]] = mapToTupleValue( t => t.left, t => t.right )
    def listMapRight: Map[B, List[A]] = mapToTupleValue( t => t.right, t => t.left )
    private def mapToTupleValue[K, J](makeKey: T => K, makeValue: T => J) : Map[K, List[J]] =
      tuples.groupMap{ case t: T => makeKey(t) } { case t: T => makeValue(t) }

    private def tuple[K](key: K, tuple: (A,B)) = (key, tuple)
    private  def tupleMap[K](makeKey: ((A, B)) => K): Map[K, (A,B)] =
      tuples.map { t =>
        val key = makeKey(t.left, t.right)
        tuple(key, t)
      }.toMap
  }
}
