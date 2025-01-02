package com.guardian.advent

import scala.collection.AbstractSeq

trait SolutionHelpers {

  implicit class RichBoolean(boolean: Boolean) {
      final def toOption[A](a: => A): Option[A] = if(boolean) Some(a) else None
  }

  implicit class RichString(string: String) {
     final def toStringList(separator: Char): List[String] = string.split(separator).toList
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
    def equalsLeft(other: (A, B)): Boolean = other.left == left
    def equalsRight(other: (A, B)): Boolean = other.right == right
  }

  implicit class RichTupleList[A,B](tuples: List[(A, B)]) {
    def tupleListToMap : Map[String, (A, B)] = tupleList[String](t => t.key)
    def tupleMapLeft: Map[A, (A, B)] = tupleList{ t => t.left }
    def tupleListRight: Map[B, (A, B)] = tupleList{ t => t.right }

    
    private def tuple[K](key: K, tuple: (A,B)) = (key, tuple)
    private  def tupleList[K](makeKey: ((A, B)) => K): Map[K, (A,B)] =
      tuples.map { t =>
        val key = makeKey[K](t.left, t.right)
        tuple(key, t)
      }.toMap
  }
}
