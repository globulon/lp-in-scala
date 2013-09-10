package math.lp

trait Simplex {
  self: Domains with Vectors with Matrices =>

  protected type Vec[A] = Vector[Int, A]
  protected type Mat[A] = Matrix[Int, Int, A]

  protected trait Dictionary[C] {
    def b: Vec[C]

    def z: Vec[C]

    def a: Mat[C]
  }

  protected def dictionary[C : Numeric](bs: Vec[C], zs: Vec[C], as: Mat[C]): Dictionary[C] = new Dictionary[C] {
    def b = bs

    def z = zs

    def a = as
  }
}
