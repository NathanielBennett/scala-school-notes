import java.time.LocalDate
import java.time.format.DateTimeFormatter

val d = LocalDate.now()
d.format(DateTimeFormatter.ISO_DATE)