package com.guardian.advent

import scala.io.Source
import scala.util.Try



object TestObject extends App {

  import scala.io.Source
  import scala.util.Try

  case class Bag(redCubeCount: Int, greenCubeCount: Int, blueCubeCount: Int)
  case class Round(redCubeCount: Int, greenCubeCount: Int, blueCubeCount: Int) {
    def playable(bag: Bag): Boolean
    = bag.redCubeCount <= this.redCubeCount && bag.greenCubeCount <= this.greenCubeCount && bag.blueCubeCount <= this.blueCubeCount

    override def toString: String = s"Reds ${redCubeCount} Blue: $blueCubeCount Green: $greenCubeCount"
  }


  object Round{

    def apply(cubes: Map[String, Int]) : Round = {
      Round(
        redCubeCount = cubes.getOrElse("red", 0),
        greenCubeCount = cubes.getOrElse("green", 0),
        blueCubeCount = cubes.getOrElse("blue", 0),
      )
    }
  }

  case class Game(rounds: List[Round]) {
    def possible(bag: Bag): Boolean = rounds.forall{
      r => r.playable(bag
    }
  }

  def parseGame(gamesString: String): Game = {

    val cubeR = """^(\d+)\s(\w+)""".r

    val gameString = gamesString.split(":").toList
    val rounds = (for {
      round <- gameString.reverse.head.split(";").toList.zipWithIndex.toList
      cube <- round._1.split(",").toList.map(c => (round._2 + 1, c))
    } yield cube)
      .groupBy { case (round, _) => round }
      .map { case (_, cubes) => cubes.map(_._2.trim) }.toList
      .flatMap { cubes =>
        val cubeMap = Try {
          cubes.map { cube =>
            val cubeR(number, colour) = cube
            (colour, number.toInt)
          }
        }
        cubeMap.toOption
      }
      .map { case l => Round(l.toMap) }
    Game(rounds)
  }

  val bag = Bag(redCubeCount = 12, greenCubeCount = 13, blueCubeCount = 14)
  val g = parseGame("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
  val g23 = parseGame("1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
  g.possible(bag)
  g23.possible(bag)
  /*
  Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day2test.txt").getLines().toList
   .zipWithIndex
   .toList
   .map { case(line, index) => (index + 1, parseGame(line))}
  //  .filter{ case(_, game) =>game.possible(bag) }*/
}
