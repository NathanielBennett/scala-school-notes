package com.guardian.advent.checker.shrinat

import scala.collection.mutable.{Set => MutableSet}
import scala.io.Source

case class Point(x: Int, y: Int) {
  def +(other: Point): Point = Point(this.x + other.x, this.y + other.y)
  def *(scalar: Int): Point = Point(this.x * scalar, this.y * scalar)
}


class DecemberSix extends Solution[Long] {

  case class Grid(val tiles: Map[Point, Char] ){
    def start = tiles.collectFirst{ case(p, start) => p}.get
    def width = tiles.keys.map{_.x}.max + 1
    def height = tiles.keys.map{_.y}.max + 1
    def isInBounds(p: Point): Boolean = p.x >= 0 && p.x < width && p.y >= 0 && p.y < height
  }

  override val title: String = "Guard gal"

  val directions = Seq(
    Point(0,-1),
    Point(1, 0),
    Point(0, 1),
    Point(-1, 0)
  )

  val start = '^'
  val space = '.'
  val block = '#'

  override def partOne(context: Context): Long = ???

  override def partTwo(context: Context): Long = ???

  def findRoute(grid: Grid, tilesOverride: Option[Map[Point, Char]] = None) : MutableSet[(Point, Point)] = {
     val tiles = tilesOverride.getOrElse(grid.tiles)
     val route: MutableSet[(Point, Point)] = MutableSet.empty
     var pos = grid.start
     var dirIdx = 0-
     var isLoop = false

     while(!isLoop && grid.isInBounds(pos)) {
        isLoop = !route.add(pos, directions(dirIdx))
        val nextPos = pos + directions(dirIdx)
        tiles.get(nextPos) match {
          case Some(block) =>
        }
     }

  }

  def parseInput(resourceName: String): Grid = {
   val tiles = getLines(resourceName).zipWithIndex.flatMap{
      case (line, y) =>
        line.zipWithIndex.collect { case (tile, x) => Point(x, y) -> tile}
    }.toMap
    Grid(tiles)
  }

  private def getLines(resurceName: String): List[String] = Source.fromResource(resurceName).getLines().toList
}
