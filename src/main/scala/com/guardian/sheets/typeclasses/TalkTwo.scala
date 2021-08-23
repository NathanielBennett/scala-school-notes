package com.guardian.sheets.typeclasses

object ImplicitTalkers {

  implicit object PersonWhoChats extends Talker[Person] {
    def chat(person: Person): String = s"Hi, I am ${person.firstName} ${person.lastName}!"
  }

  implicit object DogWhoChats extends Talker[Dog] {
    def chat(dog: Dog) = s"Woof! Woof! I am ${dog.name}"
  }
}

object TalkTwo extends App {

  import ImplicitTalkers._

  def chatter[A](a: A, talker: Talker[A]) = talker.chat(a)
  def impChatter[A](a: A)(implicit talker: Talker[A]) = talker.chat(a)

  chatter(Person("R obin", "Hood"), PersonWhoChats)
  chatter(Dog("Rover"), DogWhoChats)
  impChatter(Person("Robin", "Hood"))
  impChatter(Dog("Rover"))
}
