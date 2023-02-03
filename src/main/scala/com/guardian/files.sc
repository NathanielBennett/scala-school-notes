import scala.io.Source
import scala.util.Try

sealed trait Entry {
  def size: Int
  def name: String
}

case object NullEntry extends Entry {
  override def size = -1
  override def name = ""
}
case class File(override val size: Int, override val name: String) extends Entry

case class Directory(override val name: String, parentDirectory: Option[Directory] = None, entries: List[Entry] = List.empty) extends Entry {
  def isRoot: Boolean = name.equals("/")
  override def size = entries.foldLeft(0){ case(total, entry) => total + entry.size }
  def addNewChildDirectory(newDirectory: Directory): Directory = {
    //incase a file and a fir have the same name
    val files = entries.collect { case f: File => f }
    val otherDirs = entries
      .collect { case dir: Directory => dir }
      .filter { dir => dir.name != newDirectory.name }
    this.copy(entries = newDirectory :: otherDirs ::: files)
  }
}

case class CurrentWorkingDirectoryStack(directoryList: List[Directory]) {
  def currentWorkingDirectoryName = directoryList.headOption.map(_.name).getOrElse("/")
  def addNewDirectory(directory: Directory): CurrentWorkingDirectoryStack = CurrentWorkingDirectoryStack(directory :: directoryList)
  def moveDirectoryUp: CurrentWorkingDirectoryStack = CurrentWorkingDirectoryStack(directoryList.tail)
  def replaceInTail(replacement: Directory): CurrentWorkingDirectoryStack =
    CurrentWorkingDirectoryStack(directoryList.tail).replaceInStack(replacement)
  def replaceInStack(replacement: Directory): CurrentWorkingDirectoryStack = {
    val newDirectoryList = replacement :: directoryList.filter(dir => dir.name != replacement.name)
    CurrentWorkingDirectoryStack(newDirectoryList)
  }
  def maybeParent = directoryList.tail.headOption
  def root: Directory = directoryList.headOption.getOrElse(Directory("You fucked up badly"))
}

val chDirPattern = """^\$\scd\s(\S+)$""".r
val dirPattern = """^dir\s(\S+)$""".r
val filePattern = """^(\d+)\s(\S+)$""".r

def buildEntryList(
                    commands: List[String],
                    entriesForDirectories: List[(String, List[String])] = List.empty,
                    currentWorkingDirectoryStack: CurrentWorkingDirectoryStack = CurrentWorkingDirectoryStack(List.empty)) : Entry = {

  def addEntry(newEntry: String) : List[(String, List[String])] = entriesForDirectories match {
    //Bodge won't ever happen
    case Nil => (currentWorkingDirectoryStack.currentWorkingDirectoryName, List(newEntry)) :: Nil
    case head :: tail =>
      val (key, entries) = head
      (key, newEntry :: entries) :: tail
  }


  def directoryFromCommands(dirNameAndEntryList: (String, List[String])): Directory = {
    val (name, entryStrings) = dirNameAndEntryList
    val entries : List[Entry] = entryStrings.flatMap { c => Try {
        c match {
          case dirPattern(name) => Directory(name)
          case filePattern(size, name) => File(size.toInt, name = name)
        }
      }.toOption
    }
    Directory(name, currentWorkingDirectoryStack.maybeParent, entries)
  }

  commands match {
    case Nil =>
      //Tidy
      currentWorkingDirectoryStack.root
    case head :: tail => head match {
      case "$ ls" => buildEntryList(tail, entriesForDirectories, currentWorkingDirectoryStack)
      case chDirPattern(newWorkingDirectory) if newWorkingDirectory == ".." =>
        val thisDirectory = directoryFromCommands(entriesForDirectories.head)
        buildEntryList(tail, entriesForDirectories.tail, currentWorkingDirectoryStack.replaceInTail(thisDirectory))
      case chDirPattern(newWorkingDirectory) => buildEntryList(tail, (newWorkingDirectory, List.empty) :: entriesForDirectories, currentWorkingDirectoryStack.addNewDirectory(Directory(newWorkingDirectory)))
      case fileOrDir => buildEntryList(tail, addEntry(fileOrDir), currentWorkingDirectoryStack)
    }
  }
}


val input = Source.fromFile(s"${System.getProperty("user.home")}/test_files")
  .getLines()
  .toList


val e = buildEntryList(input)