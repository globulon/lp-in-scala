package math.lp

trait Numerics {
  protected def positive[C](implicit n: Numeric[C]): C => Boolean = n.gt(_, n.zero)

  protected def negative[C](implicit n: Numeric[C]): C => Boolean = n.lt(_, n.zero)


}
