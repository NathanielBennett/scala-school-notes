import com.guardian.sheets.{Circle, Rectangle, Shape, Square, Triangle}
import com.sun.javafx.fxml.builder.TriangleMeshBuilder

import scala.util.Random

val s: Option[String] = Some("Sunshine")
val none: Option[String] = None

def checkOption(maybeString: Option[String]) = maybeString match {
  case Some(stringValue) =>
    println(s"Morning $stringValue")
  case None =>
    println("You gave me nothing I give you nothing it return")
}

checkOption(s)
checkOption(none)

def checkOptionValue(maybeString: Option[String]) = maybeString match {
  case Some("Sunshine") =>
    println("It is a day of glory and wander")
  case Some(stringValue) =>
    println(s"the weather is: $stringValue")
  case None =>
    println("There is no weather. I hope you're in a space suit")
}

checkOptionValue(s)
checkOptionValue(Some("Hurricane"))
checkOptionValue(none)

def checkShapeType(shape: Shape) = shape match {
  case circle: Circle =>
    println(s"Got a circle with area: ${circle.area}")
  case square: Square =>
    println(s"Got a circle with area: ${square.area}")
  case triangle: Triangle =>
    println(s"Got a triangle with ${triangle.area}")
  case rectangle: Rectangle =>
    println(s"Got a triangle with ${rectangle.area}")
}

def checkCircle(shape: Shape) = shape match {
  case circle: Circle =>
    println(s"Got a circle!")
  case _ =>
    println(s"Got another shape ")
}

def checkShapeAttributes(shape: Shape) = shape match {
  case Square(h) =>
    println(s"Got a square with a height of $h")
  case Circle(r) =>
    println(s"Got a circle with a radius of $r")
  case Triangle(b, h) =>
    println(s"Got a triangle with a base: $b and a height: $h")
  case Rectangle(width, height) =>
    println(s"Got a rectangle with width: $width and $height")
}

def isBigCircle(shape: Shape) = shape match {
  case circle: Circle if circle.radius > 15 =>
    println(s"This is a big one with a radius of ${circle.radius}")
  case circle: Circle =>
    println(s"Just a circle with Radius of circle ${circle.radius}")
  case _ =>
   println("This isn't even a circle")
}

def rateMyShape(shape: Shape): Int = {
  println("this means the method needs braces")
  shape match {
    case _: Circle => 100
    case _: Square => 10
    case _: Rectangle => 1
    case _: Triangle => 11
  }
}


val square: Shape = Square(4)
val circle: Shape = Circle(7)
val triangle: Shape = Triangle(7, 3)
val rectange: Shape = Rectangle(10, 4)

checkShapeType(square)
checkShapeType(circle)

checkCircle(circle)
checkCircle(square)

checkShapeAttributes(triangle)
checkShapeAttributes(rectange)

isBigCircle(Circle(111))
isBigCircle(Circle(9))

val circleScore = rateMyShape(circle)
val rectangle = rateMyShape(rectange)