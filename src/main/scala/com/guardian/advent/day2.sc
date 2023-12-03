import scala.io.Source
import scala.util.Try

case class Bag(redCubeCount: Int, greenCubeCount: Int, blueCubeCount: Int) {
  def power = redCubeCount * greenCubeCount * blueCubeCount
}
case class Round(redCubeCount: Int, greenCubeCount: Int, blueCubeCount: Int) {
  def playable(bag: Bag): Boolean
    = this.redCubeCount <= bag.redCubeCount && this.greenCubeCount <= bag.greenCubeCount && this.blueCubeCount <= bag.blueCubeCount

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
  def possible(bag: Bag): Boolean = rounds.forall{ r => r.playable(bag) }
  def minBag(): Bag = {
    Bag(
      redCubeCount = rounds.maxBy(_.redCubeCount).redCubeCount,
      greenCubeCount = rounds.maxBy(_.greenCubeCount).greenCubeCount,
      blueCubeCount = rounds.maxBy(_.blueCubeCount).blueCubeCount
    )
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

val lines = Source.fromFile(s"${System.getProperty("user.home")}/advent2023/day2.txt").getLines().toList

val playableGames =  lines.zipWithIndex
 .toList
 .map { case(line, index) => (index + 1, parseGame(line))}
 .filter{ case(_, game) =>game.possible(bag) }
 .foldLeft(0){ case(total, (gameNo, _) ) => total + gameNo }

val minBags = lines.map{ line => parseGame(line).minBag().power }
  .foldLeft(0){ case(total, power) => total + power }