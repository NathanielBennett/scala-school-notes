
import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Try

val empty = """^(\s{3,4})$""".r
val crate = """^\[([A-Z])\]\s?$""".r

case class Stacks(stackMap: Map[Int, List[String]] = Map.empty) {

  def addCrate(newCrate: String, index: Int): Stacks = {
      val newStack = stackMap.get(index).map(stack => newCrate :: stack).getOrElse(List(newCrate))
      val newStackMap = stackMap + (index -> newStack)
      Stacks(newStackMap)
  }

  def moveCrates(from: Int, to: Int, total: Int):  Stacks = {
    val fromStack = stackMap.get(from).getOrElse(List.empty)
    val toStack = stackMap.get(to).getOrElse(List.empty)
    val leaveLength = fromStack.length - total
    Stacks(
      stackMap + (
        from -> fromStack.takeRight(leaveLength),
        to -> (fromStack.take(total) ::: toStack)
      )
    )
  }

  def whatsOnTop(): Unit = {
    val sorted = ListMap(stackMap.toSeq.sortBy(_._1):_*)
    sorted.foreach {
      case (stackIndex, stack) =>
        val topCrate = stack.headOption.map(c => s"[$c]").getOrElse(s"{EMPTY")
        println(s"Stack $stackIndex Top Crate: $topCrate")
    }
  }
}


val input = Source.fromFile(s"${System.getProperty("user.home")}/stacks.txt").getLines()
  .toList

val stackConfigurationInput = input.takeWhile(s => !s.isEmpty)
  .reverse
  .tail

val initialStackConfiguration: Stacks =  stackConfigurationInput.foldLeft(new Stacks()){ case( stackMap, line) =>
     val rawCrates = line
       .grouped(4)
       .toList
       .zipWithIndex
       .foldLeft(stackMap) {
         case (innerStacMap, (s, index)) => s match {
           case empty(_) => innerStacMap
           case crate(c) => innerStacMap.addCrate(c, index + 1)
         }
       }
     rawCrates
  }

val instructionsM = """^move ([0-9]+) from ([0-9]+) to ([0-9]+)$""".r

def parseInstruction(line: String): Option[(Int, Int, Int)] = line match {
  case instructionsM(totalS, fromS, toS) =>
    (for {
      from <- Try(fromS.toInt)
      to <- Try(toS.toInt)
      total <- Try(totalS.toInt)
    } yield (from, to, total) ).toOption
  case _ => None
}

input.takeRight(input.length - stackConfigurationInput.length)
  .map {
    line => parseInstruction(line)
  }
  .flatten
  .foldLeft(initialStackConfiguration) {
    case(currentStack, (from, to, total)) => currentStack.moveCrates(from, to, total)
  }.whatsOnTop