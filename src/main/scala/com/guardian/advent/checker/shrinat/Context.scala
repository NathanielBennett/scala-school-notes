package com.guardian.advent.checker.shrinat

trait Context {
  val input: String
  def progress(value: Double): Unit = {}
}

abstract class Solution[T] {

  val title: String
  def partOne(context: Context): T
  def partTwo(context: Context): T
  var println: Any => Unit = msg => Predef.println(msg)
}

sealed class SolutionInfo[T](day: Int, createInstance: => Solution[T]) {
  override def toString: String = s"Day: $day"
}

