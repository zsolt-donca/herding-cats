package org.zsd.cats

object Box2 extends App {

  def sumIntArray(arr: Array[Int]): Int = {
    var sum = 0

//    var i = 0
//    while (i < arr.length) {
//      sum += arr(i)
//      i += 1
//    }
    for (elem <- arr) sum += elem
    sum
  }

  val nums = (1 to 100).toArray
  val sum = sumIntArray(nums)
  println("sum: " + sum)
}
