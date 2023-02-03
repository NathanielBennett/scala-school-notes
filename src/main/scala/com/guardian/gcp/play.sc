


import scala.io.Source

sealed trait Entry {
  def size: Int
  def name: String = ""
}

case class File(override val size: Int, override val name: String) extends Entry

case class Directory(override val name: String, parentDirectory: Option[Directory] = None, entries: List[Entry] = Nil) extends Entry {
  override def size = entries.foldLeft(0){ case(total, entry)  => total + entry.size  }
}


val chDirPattern = """^\$\scd\s(\S+)$""".r
val dirPattern = """^dir\s(\S+)$""".r
val filePattern = """^(\d+)\s(\S+)$""".r

def processCommands(commands: List[String], currentWorkingDirectory: List[String] = List.empty, accumilatedEntries: List[(String, Entry)] = List.empty ) : List[(String, Entry)] = {

  lazy val makePath: String = s"/${currentWorkingDirectory.reverse.tail.mkString("/")}"

  commands match {
    case Nil => accumilatedEntries
    case head :: tail => head match {
        case filePattern(size, name) =>
          processCommands(tail, currentWorkingDirectory, (makePath, File(size.toInt, name)) :: accumilatedEntries)
        case dirPattern(name) =>
          processCommands(tail, currentWorkingDirectory, (makePath, Directory(name)) :: accumilatedEntries)
        //cheat
        case chDirPattern(name) if name == ".." =>
          processCommands(tail, currentWorkingDirectory.tail, accumilatedEntries)
        case chDirPattern(name) => processCommands(tail, name :: currentWorkingDirectory, accumilatedEntries)
        case _ => processCommands(commands.tail, currentWorkingDirectory, accumilatedEntries)
      }
  }
}

/*
def pathsToSizes(pathsRemaining: List[String], currentEntryTable: Map[String, List[Entry]] cwd: String = "/", accumulatedSizes: List[(String, Int)]): List[(String, Int)] = {
  def sizesForDir(directory: Directory, sizesSoFar: List[(String, Int)]) : List[(String, Int)] = {
    
  }
}
*/


val input = Source.fromFile(s"${System.getProperty("user.home")}/files.txt")
    .getLines()
    .toList

val entryTable = processCommands(input)
  .groupBy{ case(path, _)  => path}
  .map{ case(path, entries) => (path -> entries.map(_._2).filter(e => e.name!= path))}


e

val allPaths = entryTable.toList
  .sortBy{case(path,_) => path}
  .map{case(path, _) => path}




val headPaths = allPaths.headOption.flatMap(entryTable.get(_))
//entryTable.get("/").flatMap(entry => entry.ma)

//root = entryTable