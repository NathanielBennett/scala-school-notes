package com.guardian.scratch


import java.io.File
import java.nio.file.{FileSystems, Files, Path, Paths, StandardOpenOption}
import scala.collection.JavaConverters._
import scala.util.Try

object DirRenamber extends App {

  val path = "/home/nathaniel/working/data-platform-models/dbt/models/analytics_outsourcing"
  val dir = FileSystems.getDefault.getPath(path)
  val fileNameM = """^infomineo_(.*)$""".r


  Files.list(dir).iterator.asScala.toList
    .flatMap { rawPath =>
      Try {
        val fileNameM(rawFileName) = rawPath.getFileName.toString
        rawFileName
      }.map{
        case rawFileName => (rawFileName, rawPath)
      }.toOption
    }.foreach {
      case(rawFilename, rawPath) =>
        val path = rawPath.getParent
        val oldFileName = rawPath.getFileName
        val newFileName = s"analytics_outsourcing_$rawFilename"
        println(s"Renaming $path/$oldFileName to $path/$newFileName")
      //  new File(s"$path/$oldFileName").renameTo(new File(s"$path/$newFileName"))
    }


  //println(s"Raw files: ${rawFile.size}")
//  println(s"Rename files: ${renamed.size}")
  /*
    .foreach{
      case (path, file) =>
        println(path)
        println(file)
        println("___")
    }
*/


  /*.foreach {
    f =>
      println(f)
      println(f.getFileName)
      println(f.getParent)
      println("----")
  }*/

  def fileSegment(path: Path): Option[String] = Try {
    val fileNameM( fileName ) = path.toString
    s"$fileName"
  }.toOption

}

object MakeDeletable extends App {

  val path = "/home/nathaniel/working/gcp-iac-terraform/environments/lakehouse/bigquery/datasets/analytics_outsourcing"
  val ignore = Set("_access.yaml", "_config.yaml")

  val dir = FileSystems.getDefault.getPath(path)

  Files.list(dir).iterator.asScala
    .filter{ p => !ignore.contains(p.getFileName.toString) }
    .foreach{
      p =>
        Files.write(
          Paths.get(p.toUri),
          "\nprod_only: true\n".getBytes,
          StandardOpenOption.APPEND
        )

    }


}

