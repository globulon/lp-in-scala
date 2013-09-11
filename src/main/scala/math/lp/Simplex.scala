package math.lp

trait Simplex {
  self: Numerics with Domains with Vectors with Matrices =>

  protected type Vec = Vector[Int, BigDecimal]
  protected type Mat = Matrix[Int, Int, BigDecimal]

  protected trait Dictionary {
    def b: Vec

    def z: Vec

    def z0: BigDecimal

    def a: Mat
  }

  protected def dictionary(bs: Vec, zz: BigDecimal, zs: Vec, as: Mat): Dictionary = new Dictionary {
    def b = bs

    def z = zs

    def z0 = zz

    def a = as
  }

  protected def selectEnteringVar(d: Dictionary): Int = selectEnteringVar(enteringVars(d))

  private def enteringVars(d: Dictionary) = filterValues(d.z) { positive }

  private def selectEnteringVar(v: Vec): Int = (v.entries map (_._1)).min

  private def enteringCoef(n: Int, d: Dictionary) = filterValues(col(n, d.a)) { negative }

  protected def leavingVars(entering: Int, d: Dictionary) =
    map(enteringCoef(entering, d)) { (index, c) => -(d.b(index) / c) }

  protected def selectLeavingVar(vars: Vec): Option[Int] = vars.size match {
    case 0 => None
    case _ => vars.entries.toSeq.sortWith(sortLeavingVar).headOption map (_._1)
  }

  protected def selectLeavingVar(entering: Int, d: Dictionary): Option[Int] =
    selectLeavingVar(leavingVars(entering, d))

  private def sortLeavingVar: ((Int, BigDecimal), (Int, BigDecimal)) => Boolean = {
    case ((i1, v1), (i2, v2)) if v1 == v2 => i1 < i1
    case ((i1, v1), (i2, v2))             => v1 < v2
  }

  protected def updatez0(entering: Int, leaving: Int, d: Dictionary) = d.z0 - d.b(leaving) / d.a((leaving, entering))
}
