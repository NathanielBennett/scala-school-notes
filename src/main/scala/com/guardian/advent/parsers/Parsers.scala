package com.guardian.advent.parsers

import com.guardian.advent.AdventOfCodeParser

import scala.collection.AbstractSeq
import scala.util.Try

trait IntegerListParser extends AdventOfCodeParser[List[Int], List[List[Int]]]  {

  override def toSeq(list: List[String]): AbstractSeq[String] = list

  override def sequenceToCollection(seq: Seq[List[Int]]): List[List[Int]] = seq.toList

  protected def listToIntList(list: List[String]): Option[List[Int]] = Try {
    list.map(_.trim).map{ s => s.toInt }
  }.toOption
}

trait StringParser extends AdventOfCodeParser[String, List[String]] {
  override def lineParser(line: String): Option[String] = Some(line)

  override def toSeq(list: List[String]): AbstractSeq[String] = list

  override def sequenceToCollection(seq: Seq[String]): List[String] = seq.toList
}

trait IntegerTupleParser extends AdventOfCodeParser[(Int, Int), List[(Int, Int)]] {

  override def sequenceToCollection(seq: Seq[(Int, Int)]): List[(Int, Int)] = seq.toList

  override def toSeq(list: List[String]): AbstractSeq[String] = list

  protected def listToTuple(list: List[String]): Option[(Int, Int)] = {
    list.map(_.trim) match {
      case head :: next :: _ => Try { (head.toInt, next.toInt ) }.toOption
      case _ => None
    }
  }
}