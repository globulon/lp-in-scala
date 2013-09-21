package lecture

import math.lp._

object Week3 extends Simplex with Numerics with Matrices with Vectors with Domains{
  def main(args: Array[String]) {

    val D = dictionary(
      sparseVector(domain(Set(0,4,5,7)), Map(0 -> 20, 4 -> 25, 5 -> 10, 7 -> 23)),
      -20, sparseVector(domain(Set(1,2,3,6)), Map(1 -> -1, 3 -> 1, 6 -> -1)),
      matrix((Set(0,4,5,7), Set(1,2,3,6)), Map(
        (0,1) -> 1, (0, 2) -> 0,  (0,3) -> -1,  (0,6) -> 1,
        (4,1) -> 2, (4, 2) -> 1 , (4,3) -> -1, (4, 6) -> 1,
        (5,1) -> 0, (5, 2) -> 2 , (5,3) -> -2, (5, 6) -> 1,
        (7,1) -> 0, (7, 2) -> -1, (7,3) -> -2, (7, 6) -> 1
      )))

    loopPivot(D) match {
      case (steps, Done(r)) =>
        println(r.z0)
        println(r.b.entries.toList)
      case other => println(other)
    }

  }
}
