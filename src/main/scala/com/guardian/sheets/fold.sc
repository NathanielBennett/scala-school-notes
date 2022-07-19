import java.time.LocalDate

class Base {
  def id: Int = 1
  def date: LocalDate = LocalDate.now
}


object Extra extends Base {
  override def id: Int = 111
}


