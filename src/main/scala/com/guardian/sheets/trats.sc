trait Shape {
  def width: Int
  def height: Int
  def area: Int = width * height
  def colour: String
  def whatAmI = "I am a shape"
}

class Rect(borderColor: String, override val width: Int, override val height: Int) extends Shape {
  override def colour = "Red"
}

class Cirlce(val radius: Int) extends Shape {
  override def colour = "Red"

  override def whatAmI: String = "I am circle"

  override val width = radius

  override val height = radius
}

class FourSquare extends Shape {
  override def width = 2

  override def height = 2

  override def colour = "Yelllo"
}

val r = new Rect( "red",4, 5)
r.whatAmI

trait Pet {
  def speak = println("Yo")     // concrete implementation of a speak method
  def comeToMaster(): Unit      // abstract
  def gimmeShape() : Shape
}

class Dog(name: String) extends Pet {
  def comeToMaster(): Unit = {}

  override def gimmeShape() = new Cirlce(4)
}

val dog = new Dog("Rover")
val s = dog.gimmeShape()
s.whatAmI