package org.zsd.cats

import org.scalatest.{FunSuite, Matchers}
import cats._
import cats.implicits._
import cats.data.Validated
import Validated.{valid, invalid}

class Sandbox extends FunSuite with Matchers {

  test("Semigroup test") {

    Semigroup[Int].combine(1, 2) should be(3)

    Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) should be(List(1, 2, 3, 4, 5, 6))

    Semigroup[Option[Int]].combine(Option(1), Option(2)) should be(Option(3))

    Semigroup[Option[Int]].combine(Option(1), None) should be(Option(1))

    val len: String => Int = _.length
    val distLen: String => Int = _.distinct.length

    (len |+| distLen)("abc bac") should be(11)

    Semigroup[Int ⇒ Int].combine({ (x: Int) ⇒ x + 1 }, { (x: Int) ⇒ x * 10 }).apply(6) should be(67)
  }

  test("Functor") {
    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    val f1: (Int => String) = i => i.toString
    val f2: (String => Int) = _.length

    (f1 map f2)(123) should be(3)
  }

  test("Validated") {
    val v1: Validated[List[String], Int] = valid(7)
    val v2: Validated[List[String], Int] = invalid(List("Error: success"))
    val v3: Validated[List[String], Int] = invalid(List("Try harder!"))

    (v1 |@| v2 |@| v3).map(_ + _ + _) shouldBe invalid(List("Error: success", "Try harder!"))
  }

  test("Traversable") {

    List(List("1")).sequence

  }
}
