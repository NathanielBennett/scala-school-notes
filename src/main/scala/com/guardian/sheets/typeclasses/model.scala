package com.guardian.sheets.typeclasses

sealed case class Dog(name: String)
sealed case class Person(firstName: String, lastName: String)