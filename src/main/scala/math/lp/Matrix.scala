package math.lp

import scala.language.postfixOps
import fpatterns.Reader

trait Domains {
  protected type Domain[A] = Set[A]

  protected def domain[A](as: Set[A]): Domain[A] = as
}

trait Vectors {
  self: Domains =>

  protected trait Vector[A, C] extends ((A) => C) {
    def domain: Domain[A]

    def data: Map[A, C]

    def size: Int

    def isZero: Boolean = size == 0
  }

  protected def sparseVector[A, C: Numeric](d: Domain[A], m: Map[A, C]): Vector[A, C] = new Vector[A, C] {
    def domain: Domain[A] = d

    def apply(a: A) = m.getOrElse(a, implicitly[Numeric[C]].zero)

    def data = m

    def size = m.count(p => implicitly[Numeric[C]].zero != p._2)
  }

  protected def filterValues[A, C: Numeric](v: Vector[A, C])(p: C => Boolean): Vector[A, C] =
    sparseVector(v.domain, v.data.filter(e => p(e._2)))

  protected def filterKeys[A, C: Numeric](v: Vector[A, C])(p: A => Boolean): Vector[A, C] =
    sparseVector(v.domain, v.data.filter(e => p(e._1)))

  protected def map[A, C: Numeric, D: Numeric](v: Vector[A, C])(f: (A, C) => D): Vector[A, D] =
    sparseVector(v.domain, v.data.map { p => (p._1, f.tupled(p)) })

  protected def findValue[A, C: Numeric](v: Vector[A, C])(p: C => Boolean): Option[(A, C)] =
    v.data find { pair => p(pair._2) }

  protected def modifyVec[A, C : Numeric](v: Vector[A, C], a: A)(f: C => C): Vector[A, C] =
    sparseVector(v.domain, v.data + (a -> f(v(a))))

  protected def readData[A, C] = Reader[Vector[A, C], Map[A, C]] {_.data}
}

trait Matrices {
  self: Vectors with Domains =>

  protected type Data[A, B, C] = Map[(A, B), C]

  protected trait Matrix[A, B, C] extends (((A, B)) => C) {
    def domains: (Domain[A], Domain[B])

    def Row = domains._1

    def Col = domains._2

    def entries: Iterable[((A, B), C)]
  }

  protected def matrix[A, B, C: Numeric](ds: (Domain[A], Domain[B]), d: Data[A, B, C]): Matrix[A, B, C] = new Matrix[A, B, C] {
    def domains = ds

    def data = d

    def apply(k: (A, B)) = data.getOrElse(k, implicitly[Numeric[C]].zero)

    def entries = d.toIterable
  }

  protected def filterValues[A, B, C: Numeric](mat: Matrix[A, B, C])(p: C => Boolean): Matrix[A, B, C] = matrix[A, B, C](
    mat.domains, mat.entries filter (e => p(e._2)) toMap)

  protected def col[A, B, C: Numeric](b: B, mat: Matrix[A, B, C]): Vector[A, C] =
    sparseVector[A, C](mat.Row, mat.entries.filter(_._1._2 == b).map(e => (e._1._1, e._2)).toMap)

  protected def row[A, B, C: Numeric](a: A, mat: Matrix[A, B, C]): Vector[B, C] =
    sparseVector(mat.Col, mat.entries.filter(_._1._1 == a).map(e => (e._1._2, e._2)).toMap)
}
