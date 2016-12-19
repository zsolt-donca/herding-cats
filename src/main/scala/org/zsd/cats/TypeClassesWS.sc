import cats._
import cats.instances.all._
import cats.syntax.order._
import cats.syntax.show._

def sort[T: Order](ts: List[T]): List[T] = {
  ts.sortWith((t1, t2) => t1 <= t2)
}

sort(List(3, 2, 1))

case class Person(name: String)
case class Car(model: String)

implicit val personShow: Show[Person] = Show.show(p => s"Person(${p.name})")
implicit val carShow: Show[Car] = Show.show(c => s"Car(${c.model})")

def takeALook[T: Show](values: List[T]): String = {
  values.map(_.show).mkString(", ")
}

takeALook(List(Person("Alice"), Person("Bob")))
takeALook(List(Car("Tesla Model S")))

import simulacrum._

@typeclass trait CanTruthy[A] { self =>
  /** Return true, if `a` is truthy. */
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthy(a: A): Boolean = f(a)
  }
}

case class Child(name: String, age: Int)

implicit val truthyPerson: CanTruthy[Child] = CanTruthy.fromTruthy[Child](person => person.age <= 5)

def numberOfThruthies[T: CanTruthy](list: List[T]): Int = {
  import CanTruthy.ops._

  list.count(_.truthy)
}

numberOfThruthies(List(Child("Alice", 7), Child("Bob", 5)))
