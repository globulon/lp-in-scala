package math.lp

import scala.annotation.tailrec

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

  protected def selectEnteringVar(d: Dictionary): Option[Int] = selectEnteringVar(enteringVars(d))

  private def enteringVars(d: Dictionary) = filterValues(d.z) { positive }

  private def selectEnteringVar(v: Vec): Option[Int] =
    if (v.isZero) None else Some((v.entries map (_._1)).min)

  private def enteringCoef(n: Int, d: Dictionary) = filterValues(col(n, d.a)) { negative }

  protected def leavingVars(entering: Int, d: Dictionary) =
    map(enteringCoef(entering, d)) { (index, c) => -(d.b(index) / c) }

  private def selectLeavingVar(vars: Vec): Option[Int] = vars.entries.size match {
    case 0 => None
    case _ => vars.entries.toSeq.sortWith(sortLeavingVar).headOption map (_._1)
  }

  protected def selectLeavingVar(entering: Int, d: Dictionary): Option[Int] = {
    //    println(selectLeavingVar(leavingVars(entering, d)))
    selectLeavingVar(leavingVars(entering, d))
  }

  private def sortLeavingVar: ((Int, BigDecimal), (Int, BigDecimal)) => Boolean = {
    case ((i1, v1), (i2, v2)) if v1 == v2 => i1 < i2
    case ((i1, v1), (i2, v2))             => v1 < v2
  }

  private def nextb(entering: Int, leaving: Int, d: Dictionary): Vec = {
    val newRow = d.b.domain - leaving
    val newEntries = newRow.foldLeft(Map.empty[Int, BigDecimal]) { (bs, row) =>
      bs + (row -> (d.b(row) - d.a(row, entering) * d.b(leaving) / d.a(leaving, entering)))
    } + (entering -> -d.b(leaving) / d.a(leaving, entering))
    sparseVector(newRow + entering, newEntries)
  }

  private def nextz(entering: Int, leaving: Int, d: Dictionary): Vec = {
    val newCol = d.a.Col - entering
    val newEntries = newCol.foldLeft(Map.empty[Int, BigDecimal]) { (mod, x) =>
      mod + (x -> (d.z(x) - d.z(entering) * d.a((leaving, x)) / d.a((leaving, entering))))
    } + (leaving -> d.z(entering) / d.a((leaving, entering)))
    sparseVector(newCol + leaving, newEntries)
  }

  private def substitute(entering: Int, leaving: Int, newCol: Domain[Int], d: Dictionary) = (mat: Map[(Int, Int), BigDecimal], r: Int) =>
    newCol.foldLeft(mat) { (acc, c) =>
      acc + ((r, c) -> (d.a(r, c) - d.a((r, entering)) * d.a((leaving, c)) / d.a((leaving, entering))))
    } + ((r, leaving) -> d.a((r, entering)) / d.a((leaving, entering)))

  private def nexta(entering: Int, leaving: Int, d: Dictionary): Mat = {
    val newCol = d.a.Col - entering
    val newRow = d.a.Row - leaving
    val newEntries = newRow.foldLeft(Map.empty[(Int, Int), BigDecimal]) { substitute(entering, leaving, newCol, d) }
    val enteringRow = newCol.foldLeft(Map.empty[(Int, Int), BigDecimal]) { (map, col) =>
      map + ((entering, col) -> -d.a((leaving, col)) / d.a((leaving, entering)))
    } + ((entering, leaving) -> 1 / d.a((leaving, entering)))

    matrix((newRow + entering, newCol + leaving), newEntries ++ enteringRow)
  }

  protected def nextz0(entering: Int, leaving: Int, d: Dictionary) = d.z0 - d.b(leaving) * d.z(entering) / d.a((leaving, entering))

  protected type Step = Int
  protected type Cost = BigDecimal

  sealed trait PivotStatus
  protected case class Done(d: Dictionary) extends PivotStatus
  protected case class Cont(d: Dictionary) extends PivotStatus
  protected object Unbounded extends PivotStatus

  private def pivot(leaving: Int, entering: Int, d: Dictionary): PivotStatus =
    Cont(dictionary(nextb(entering, leaving, d), nextz0(entering, leaving, d), nextz(entering, leaving, d), nexta(entering, leaving, d)))

  private def pivot(d: Dictionary, entering: Int): PivotStatus = {
    selectLeavingVar(entering, d) match {
      case Some(leaving) => pivot(leaving, entering, d)
      case None =>
        Unbounded
    }
  }

  private def pivot(d: Dictionary): PivotStatus = selectEnteringVar(d) match {
    case Some(entering) => pivot(d, entering)
    case None           => Done(d)
  }

  @tailrec
  private def loopPivot(s: Step, ps: PivotStatus): (Step, PivotStatus) = ps match {
    case Cont(d) => loopPivot(s + 1, pivot(d))
    case r       => (s, r)
  }

  protected def loopPivot(d: Dictionary): (Step, PivotStatus) = loopPivot(-1, Cont(d))
}
