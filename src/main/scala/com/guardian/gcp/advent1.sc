import scala.io.Source
import scala.util.Try

val d = System.getProperty("user.home")

case class Elf(rawMeals: List[String]) {
  val totalCalories = rawMeals.flatMap{ meal => Try(meal.toInt).toOption }
    .foldLeft(0){ case (a, b) => a + b }
}

def mealsToElves(meals: List[String],
                 mealsForElf: List[String] = List.empty,
                 elves: List[Elf] = List.empty): List[Elf] = {
  meals match {
    case Nil => elves
    case "" :: tail => mealsToElves(tail, List.empty, Elf(mealsForElf) :: elves)
    case head :: tail => mealsToElves(tail, head :: mealsForElf, elves)
  }
}

val rawMealData = Source.fromFile(s"${System.getProperty("user.home")}/elven_dinners.txt").getLines().toList

val maxCalories = mealsToElves(rawMealData)
  .sortBy(elf => elf.totalCalories)
  .reverse
  .head
  .totalCalories












