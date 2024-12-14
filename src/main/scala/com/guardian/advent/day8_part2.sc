import scala.io.Source
import scala.util.Try

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day8.txt").getLines().toList

val instructions = lines.head.split("").toList

val mapEntryR = """^([A-Z0-9]{3})\s=\s\(([A-Z0-9]{3}),\s([A-Z0-9]{3})\)""".r

def readMapEntry(line: String): Option[(String, (String, String))] = Try {
  val mapEntryR(key, left, right) = line
  (key, (left, right))
}.toOption
val mapEntries =  lines.tail.flatMap{ line => readMapEntry(line) }.toMap

def nodeComponent(node: (String, String), instruction: String): String =
  if (instruction == "L") node._1 else node._2

def turn3(steps: List[String], currentKeys: List[String], stepCnt: Int = 0): Int = {
  println(s"++ ($stepCnt) $steps $currentKeys")
  currentKeys.forall{ s => s.endsWith("Z")} match {
    case true => stepCnt
    case false =>
        val nextKeys = currentKeys.flatMap{ key => mapEntries.get(key).map{ node => nodeComponent(node, steps.head)} }
        val nextSteps = steps.tail match {
          case Nil => instructions
          case x => x
        }
        turn3(nextSteps, nextKeys, stepCnt + 1)

  }
}

val startKeys = mapEntries.keys.toList.filter {  s => s.endsWith("A")  }
def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1:BigInt){
  case(a, b) => b * a /
    Stream.iterate((a, b)){ case(x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
}

def countSteps(steps: List[String], currentKey: Option[String], acc: List[Int] = List.empty, stepCnt: Int = 0): List[Int] = {
/*  println(
    s"Step: ${steps.head}"
  )*/
  if(acc.size == 10)
    return  acc.reverse
  currentKey match {
    case Some(key) =>
      val nextKey = mapEntries.get(key).map{ node => nodeComponent(node, steps.head) }
      val nextSteps = steps.tail match {
        case Nil => instructions
        case x => x
      }
      val nextAcc = key.endsWith("Z") match {
        case true => stepCnt :: acc
        case false => acc
      }
      countSteps(nextSteps, nextKey, nextAcc, stepCnt + 1)
    case _ =>  List.empty
  }
}

def diffs(list: List[Int], last: Int, acc: List[Int] = List.empty ) : List [Int] = {
  list match {
    case Nil => acc
    case head :: tail if acc.isEmpty => diffs(tail, head, head :: acc)
    case head :: tail => diffs(tail, head, head - last :: acc)
  }
}

val bigDifs = startKeys.map{ key => countSteps(instructions, Some(key)) }
  .map{ occurences => diffs(occurences, 0).head }
  .map {diff => BigInt(diff)}

lcm(bigDifs)
