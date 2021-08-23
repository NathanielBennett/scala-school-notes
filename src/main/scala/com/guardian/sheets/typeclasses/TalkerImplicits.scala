package com.guardian.sheets.typeclasses

object TalkerImplicits {
   implicit object DogTalker extends Talker[Dog] {
     override def chat(a: Dog): String = s"Woof Woof I am ${a.name}"
   }

   implicit object PersonTalker extends Talker[Person] {
     override def chat(person: Person): String = s"Hello I am ${person.firstName} ${person.lastName}"
   }

   implicit class StraighTalker[A](a: A) {
      def talk(implicit talker: Talker[A]) = talker.chat(a)
   }
}

object TalkTheTalk extends App {
  import TalkerImplicits._

  println(Dog("Rover").talk)
  println(Person("Wild","Rover").talk)
}
