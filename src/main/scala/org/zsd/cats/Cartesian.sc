import cats._
import cats.implicits._

(3.some |@| 5.some) map { _ - _ }

(List("ha", "heh", "hmm") |@| List("?", "!", ".") |@| List(1, 2)) map {_ + _ + _}

3.some <* 5.some

List((_: Int) + 1, (_: Int) * 2) ap List(2, 3, 4)