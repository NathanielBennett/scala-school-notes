import scala.io.Source
import scala.util.Try

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day8.txt").getLines().toList

val instructions = lines.head.split("").toList

val mapEntryR = """^([A-Z]{3})\s=\s\(([A-Z]{3}),\s([A-Z]{3})\)""".r

def readMapEntry(line: String): Option[(String, (String, String))] = Try {
  val mapEntryR(key, left, right) = line
  (key, (left, right))
}.toOption
val mapEntries =  lines.tail.flatMap{ line => readMapEntry(line) }.toMap

def nodeComponent(node: (String, String), instruction: String): String =
  if (instruction == "L") node._1 else node._2

def turn2(steps: List[String], currentKey: Option[String] = Some("AAA"), stepCnt: Int = 0): Int = {
//  println(s"+++($stepCnt) ${steps} :$currentKey")
  currentKey match {
    case Some("ZZZ") => stepCnt
    case _ =>
        val nextNode = currentKey.flatMap {
          key => mapEntries.get(key).map { node => nodeComponent(node, steps.head) }
        }

        val nextSteps = steps.tail match {
          case Nil => instructions
          case x => x
        }
        turn2(nextSteps, nextNode, stepCnt + 1)
  }
}
turn2(instructions)







