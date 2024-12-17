val s = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
val dont = "don't()"
val doo = "do()"
def makeNextDelim(delim: String): String = if (delim == dont) doo else dont

def end(rawInstructions: String, delim: String) = rawInstructions.indexOf(delim) < -1
def startStop(rawInstructions: String): String =
  rawInstructions.indexOf(dont) < rawInstructions.indexOf(doo) match {
    case true => dont
    case false => doo

  }

def findInstructions
   (rawInstructiona: String, delim: String, cleanInstructions: List[(String, String)] = List.empty): List[(String, String)] = {
   println(delim)

  if (rawInstructiona.isEmpty) cleanInstructions.reverse
  else {
    val nextDelim = makeNextDelim(delim)
    val nextRawInstructions = rawInstructiona.substring(rawInstructiona.indexOf(delim) + delim.length)

    if (end(rawInstructiona, delim)) findInstructions("", nextRawInstructions, (rawInstructiona, nextDelim) :: cleanInstructions)
    else {
      findInstructions(
        nextRawInstructions,
        nextDelim,
        (rawInstructiona.substring(0, rawInstructiona.indexOf(delim)), delim) :: cleanInstructions
      )
    }
  }
}

findInstructions(s, startStop(s))



