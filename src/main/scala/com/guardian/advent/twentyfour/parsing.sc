trait Test {
  def day: Int
}

trait Solution {
  def day: Int
}

class Diamond extends Test with Solution {
  override def day = 10
}

val d = new Diamond()
println(s"${d.day}")
