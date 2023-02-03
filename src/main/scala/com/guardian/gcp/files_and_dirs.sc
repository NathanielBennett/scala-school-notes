import scala.io.Source

case class File(size: Int, name: String)

val chDirPattern = """^\$\scd\s(\S+)$""".r
val filePattern = """^(\d+)\s(\S+)$""".r


def allPathsForCurrentDir(dirNames: List[String], pathsSoFar: List[String] = List.empty): List[String] = {
  dirNames match {
    case Nil => pathsSoFar
    case head :: _ if head == "/"  => head :: pathsSoFar
    case _ :: tail =>
      val newPath = s"/${dirNames.reverse.tail.mkString("/")}"
      allPathsForCurrentDir(tail, newPath :: pathsSoFar)
  }
}

def updatePaths(filesAndDirs: List[(String, List[File])], pathsAndSizes: Map[String, Int] ): Map[String, Int] = {
  val currentPath = filesAndDirs.map{case(dirName, _) => dirName}
  val totalSizeOfNewFiles = filesAndDirs
    .headOption
    .map{ case (_, files) => files.foldLeft(0){ case (acc, file) => acc + file.size } }
    .getOrElse(0)

  allPathsForCurrentDir(currentPath)
    .foldLeft(pathsAndSizes){ case (currrentPathMap, path) =>
      val currentTotalForPath = currrentPathMap.getOrElse(path, 0)
      currrentPathMap + (path -> (currentTotalForPath + totalSizeOfNewFiles) )
    }
}

def updateRoot(filesAndDirs: List[(String, List[File])], pathsAndSizes: Map[String, Int]) : Map[String, Int] =
  filesAndDirs match {
    case Nil => pathsAndSizes
    case _ :: tail =>
      val updatedPaths = updatePaths(filesAndDirs, pathsAndSizes)
      updateRoot(tail, updatedPaths)
  }

def processCommands(commands: List[String], filesAndDirs: List[(String, List[File])] = List.empty, pathsAndSizes: Map[String, Int] = Map.empty) : Map[String, Int] = {

  def addFile(file: File): List[(String, List[File])] = {
     val updated = filesAndDirs
       .headOption
       .map {case (dir, files) => (dir, file :: files)}
       .getOrElse{ ("/", List(file))}
    updated :: filesAndDirs.tail
  }

  commands match {
    case Nil =>
      updateRoot(filesAndDirs, pathsAndSizes)
    case head :: tail => head match {
      case filePattern(size, name) =>
        val updatedFilesAndDirs = addFile(File(size.toInt, name))
        processCommands(tail, updatedFilesAndDirs, pathsAndSizes )
      case chDirPattern(name) if name == ".." =>
        val updatedPaths = updatePaths(filesAndDirs, pathsAndSizes)
        processCommands(tail, filesAndDirs.tail, updatedPaths)
      case chDirPattern(name) =>
        processCommands(tail, (name, List.empty) :: filesAndDirs, pathsAndSizes)
      case _ =>
        processCommands(tail, filesAndDirs, pathsAndSizes)
    }
  }
}

val input = Source.fromFile(s"${System.getProperty("user.home")}/files.txt")
  .getLines
  .toList

processCommands(input)
  .toList
  .filter{case(_,  size) => size < 100000}
  .foldLeft(0){case(acc, (_, size)) => acc + size }



