package fpatterns

import scala.language.higherKinds

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(zero: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(zero: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (acc, a) => mb.op(acc, f(a)) }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}

trait Foldables {
  protected def foldableSequence[A]: Foldable[Seq] = new Foldable[Seq] {
    def foldRight[A, B](as: Seq[A])(zero: B)(f: (A, B) => B): B = as.foldRight(zero)(f)

    def foldLeft[A, B](as: Seq[A])(zero: B)(f: (B, A) => B): B = as.foldLeft(zero)(f)
  }
}
