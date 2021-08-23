package com.guardian.sheets.typeclasses

object PersonWhoChats {
  def chat(person: Person): String = s"Hi, I am ${person.firstName} ${person.lastName}!"
}

object DogWhoChats {
  def chat(dog: Dog) = s"Woof! Woof! I am ${dog.name}"
}


object TalkOne extends App {

  PersonWhoChats.chat(Person("Henry","DoGooder"))
  DogWhoChats.chat(Dog("Bowser"))
}
