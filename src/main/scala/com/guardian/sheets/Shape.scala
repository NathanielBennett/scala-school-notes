package com.guardian.sheets

sealed trait Shape {
  def area(): Double
}

case class Square(height: Int) extends Shape {
  override def area(): Double = height * height
}

case class Triangle(base: Int, height: Int) extends Shape {
  override def area(): Double = base * height / 2
}

case class Rectangle(width: Int, height: Int) extends Shape {
  override def area(): Double = width * height
}

case class Circle(radius: Int) extends Shape {
  override def area(): Double = Math.PI * (radius * radius)
}https://www.coursera.org/search?query=Project%20Manager