package com.guardian.advent

trait SolutionHelpers {

  implicit class RichBoolean(boolean: Boolean) {
      final def toOption[A](a: => A): Option[A] = if(boolean) Some(a) else None
  }

  implicit class RichString(string: String) {
     final def toStringList(separator: Char): List[String] = string.split(separator).toList
  }

  implicit def gridEntryListToString[A](gridEntries: Seq[GridEntry[A]]): String = {
     gridEntries.foldLeft(new StringBuilder()) { case (strinBuilder,a) => strinBuilder.append(a.value)  }.toString
  }
}
