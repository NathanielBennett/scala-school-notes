package com.guardian.advent.util

import java.nio.file.{FileSystem, FileSystems, Files, LinkOption, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._


class ResourceMover (basepath: String) {
  lazy val fileSystem = FileSystems.newFileSystem(getClass.getResource(basepath).toURI, Map[String, String]().asJava)


  def listFilesFromResource(folder: String): List[Path] = {
    Files.list(getPathForResource(folder))
      .filter{ p => Files.isRegularFile(p, Array[LinkOption](): _*) }
      .sorted
      .iterator
      .asScala
      .toList
  }

  private def getPathForResource(fileName: String): Path = {
    val url = classOf[ResourceMover].getResource(s"$basepath/$fileName")
    if ("file" == url.getProtocol) Paths.get(url.toURI)
    else fileSystem.getPath(basepath, fileName)
  }
}

object ResourceTidier extends App {

  val mover = new ResourceMover("advent2024")
  println(mover.listFilesFromResource("debug"))

}
