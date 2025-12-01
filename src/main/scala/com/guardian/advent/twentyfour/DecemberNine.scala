package com.guardian.advent.twentyfour

import com.guardian.advent.December
import com.guardian.advent.parsers.StringParser

import scala.util.Try

case class  DiskEntry(capacity: Int, contents: List[Int] = List.empty) {

  def fileIndex : Option[Int] = contents match {
    case Nil => None
    case head :: _ => Some(head)
  }

  def addFile(file: Int): DiskEntry = {
    val newContents = contents match {
      case Nil => List(file)
      case x => x.appended(file)
    }
    this.copy( contents = newContents  )
  }

  def addFiles(files: List[Int]): DiskEntry = this.copy(contents = this.contents ::: files)
  def takeFile(): (DiskEntry, Int) = (this.copy( contents = this.contents.take(contents.length - 1)),  this.contents.last)
  def isEmpty: Boolean = contents.isEmpty
  def capacityRemaining = capacity - contents.length

  def toStringPadded: String = contents match {
    case Nil => List.fill(capacity)(".").mkString
    case _ => contents.mkString.padTo(capacity, '.')
  }

  def contentsAsChars: List[Option[Int]] = contents.map{ f => Some(f) }.padTo(capacity, None)
}

trait DecemberNine extends December[Long, List[String], (Int, Int)] with StringParser {

  override def day: Int = 9

  override def solver: Solver[(Int, Int), Long] = new Solver[(Int, Int), Long] {
    override def solution(list: List[(Int, Int)]): Long = list.foldLeft(0L) {
      case (a, b) => a + (b.left.toLong * b.right.toLong)
    }
  }

  val lines = rawInput.mkString

  def entriesToString(diskEntries: List[DiskEntry]): String = {
    diskEntries.foldLeft(new StringBuilder()) {
      case (stringBuilder, diskEntry) => stringBuilder.append(diskEntry.toStringPadded)
    }.toString
  }

  def shiftFiles(input: String): List[DiskEntry]

  def filesAndDisk(input: String): (List[DiskEntry], Int) = {

    def makeFileBlock(content: Int, capacity: Int): Option[DiskEntry] = {
      Some(DiskEntry(content, List.fill(content)(capacity)))
    }

    def loop(remaining: List[Int], fileIndex: Int = 0, acc: List[Option[DiskEntry]] = List.empty): (List[DiskEntry], Int) = {
      remaining match {
        case Nil => (acc.flatten, fileIndex - 1)
        case head :: Nil =>
          val last = makeFileBlock(head, fileIndex)
          loop(Nil, fileIndex + 1, acc ::: List(last))
        case head :: next :: tail =>
          val nextDiskEntries = List(
            makeFileBlock(head, fileIndex),
            if (next > 0) Some(DiskEntry(next))
            else None
          )
          loop(tail, fileIndex + 1, acc ::: nextDiskEntries)
      }
    }

    val rawDiskData = input.map { c => c.toString }
      .flatMap { ch => Try {Integer.parseInt(ch)}.toOption }.toList

    loop(rawDiskData)
  }

  def getIndexedBlocks(diskEntries: List[DiskEntry]): List[(Int, Int)] = {

    diskEntries.flatMap { diskEntry => diskEntry.contentsAsChars }
      .zipWithIndex
      .flatMap { case(maybeBlock, index) =>  maybeBlock.map{ block => (block, index) } }
  }

  override def rawSolution: List[(Int, Int)] = {
    val shifted = shiftFiles(lines)
    getIndexedBlocks(shifted)
  }
}

trait DecemberNinePartOne extends DecemberNine {

  override def shiftFiles(input: String): List[DiskEntry] = {

    def findFileAndSpace(currentEntries: List[(DiskEntry, Int)]): Option[((DiskEntry, Int), (DiskEntry, Int))] = {
      (for {
        lastFile <- currentEntries.findLast{ case (d, _ ) => !d.isEmpty }
        space <- currentEntries.find{ case (d, _) => d.capacityRemaining > 0 }
      } yield (space, lastFile))
    }

    def updateList(currentEntries: List[(DiskEntry, Int)]) : List[DiskEntry] = {
      (for {
        (spaceEntry, fileEntry) <- findFileAndSpace(currentEntries)
      } yield {
        val (moved, movedIndex) = fileEntry
        val (space, spaceIndex) = spaceEntry
        val (updatedMoved, file) = moved.takeFile()
        currentEntries.map { case (entry, index) =>
          index match {
            case index if index == movedIndex => updatedMoved
            case index if index == spaceIndex => entry.addFile(file)
            case _ => entry
          }
        }
      }).getOrElse(currentEntries.map{case(d, _) => d})
    }

    def allFilesShifted(entries: List[DiskEntry]): Boolean = {
      val filled = entries.takeWhile{ diskEntry => !diskEntry.isEmpty }.size
      entries.
        takeRight(entries.length - filled)
        .forall{ diskEntry => diskEntry.isEmpty}
    }

    def loop(currentEntries: List[DiskEntry]) : List[DiskEntry] = {
      val done = allFilesShifted(currentEntries)
      if(done) currentEntries.takeWhile{ de => !de.isEmpty }
      else {
        val nextEntries = updateList(currentEntries.zipWithIndex)
        loop(nextEntries)
      }
    }
    val (entries, _) = filesAndDisk(lines)
    loop(entries)
  }
}

object DecemberNinePartOneTest extends DecemberNinePartOne with PuzzleTest
object DecemberNinePartOneSolution extends DecemberNinePartOne with PuzzleSolution

trait DecemberNinePartTwo extends DecemberNine {

  override def shiftFiles(input: String): List[DiskEntry] = {

    def findFileAndSpace(currentEntries: List[(DiskEntry, Int)], currentMaxIndex: Int): Option[((DiskEntry, Int, Int, Int))] = {
      for {
        (fileToMove,  moveIndex) <- currentEntries.find { case (de, _) => de.fileIndex == Some(currentMaxIndex) }
        (space, spaceIndex) <- currentEntries.find { case (de, index) => de.capacityRemaining >= fileToMove.capacity && index < moveIndex }
      } yield {
        val moved = space.addFiles(fileToMove.contents)
        (moved, moveIndex, spaceIndex, fileToMove.capacity)
      }
    }

    def updateList(currentEntries: List[(DiskEntry, Int)], currentMaxIndex: Int): List[(DiskEntry, Int)] = {
      (for {
        (moved, movedIndex, spaceIndex, spaceCapacity) <- findFileAndSpace(currentEntries, currentMaxIndex)
      } yield {
           currentEntries.map {
             case (entry, index) =>
              index match {
                 case index if index == spaceIndex => (moved, index)
                 case index if index == movedIndex => (DiskEntry(spaceCapacity), index)
                 case _ => (entry, index)
               }
           }
        }).getOrElse(currentEntries)
    }

    def loop(currentEntries: List[(DiskEntry, Int)], currentMaxIndex: Int): List[DiskEntry] = {
       if(currentMaxIndex == 0) currentEntries.map{case(diskEntry, _) => diskEntry }
       else {
         val nextEntries = updateList(currentEntries, currentMaxIndex)
         loop(nextEntries, currentMaxIndex - 1)
       }
    }

    val (entries, maxIndex) = filesAndDisk(input)
    loop(entries.zipWithIndex, maxIndex)
  }
}

object DecemberNinePartTwoTest extends DecemberNinePartTwo with PuzzleTest
object DecemberNinePartTwoSolution extends DecemberNinePartTwo with PuzzleSolution




