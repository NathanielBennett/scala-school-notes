package com.guardian.advent

import java.io.{BufferedWriter, FileWriter}
import scala.io.Source

object FileSorter extends App {

  val f = "/home/nathaniel/working/data-platform-models/dbt/dictionary.txt"
  val new_words = List(
    "getdbt",
    "configs",
    "terraform",
    "lakehouse",
    "auxia",
    "fielda",
    "ixoh",
    "okta"
  )

  val allWwords = Source.fromFile(f).getLines().toList
  println(s"Old: ${allWwords.size}")
  val newWords = allWwords.head :: (allWwords.tail ::: new_words).sorted
  println(s"New ${newWords.size} ")

  val Writer = new BufferedWriter(new FileWriter(f))
  newWords.foreach{
    w => Writer.write(w)
    Writer.newLine()
  }
  Writer.flush()
  Writer.close()
  println("Done")
}
