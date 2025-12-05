package com.guardian.advent.parsers

import com.guardian.advent.AdventOfCodeParser

import scala.collection.AbstractSeq
import scala.util.Try

trait IntegerListParser extends AdventOfCodeParser[List[Int], List[List[Int]]]  {

  def separator = ""

  override def sequenceToCollection(seq: Seq[List[Int]]): List[List[Int]] = seq.toList

  protected def listToIntList(list: List[String]): Option[List[Int]] = Try {
    list.map(_.trim).map{ s => s.toInt }
  }.toOption

  override def lineParser(line: String): Option[List[Int]] = Try {
    line.split(separator).map(_.toInt).toList
  }.toOption
}


trait StringParser extends AdventOfCodeParser[String, List[String]] {
  override def lineParser(line: String): Option[String] = Some(line)

  override def sequenceToCollection(seq: Seq[String]): List[String] = seq.toList
}

trait IntegerTupleParser extends AdventOfCodeParser[(Int, Int), List[(Int, Int)]] {

  override def sequenceToCollection(seq: Seq[(Int, Int)]): List[(Int, Int)] = seq.toList

  protected def listToTuple(list: List[String]): Option[(Int, Int)] = {
    list.map(_.trim) match {
      case head :: next :: _ => Try { (head.toInt, next.toInt ) }.toOption
      case _ => None
    }
  }
}

trait GenericTupleParser[A, B] extends AdventOfCodeParser[(A, B), List[(A, B)]]  {

  def stringToA(s: String): A
  def stringToB(s: String): B

  override def sequenceToCollection(seq: Seq[(A, B)]): List[(A, B)] = seq.toList

  protected def listToTuple(list: List[String]): Option[(A, B)] = {
    list.map(_.trim) match {
      case head :: next :: _ => Try {
        ( stringToA(head), stringToB(next ))
      }.toOption
    }
  }
}

trait GenericSingleLineTupleParser[A, B] extends GenericTupleParser[A, B]  {

  def separate(string: String): List[String]

  override def rawInput: List[(A, B)] = {
    val rawLines = getLines().mkString
    val splitLines = separate(rawLines)
    parseLinesFromResource(splitLines)
  }
}

