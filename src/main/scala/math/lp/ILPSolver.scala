package math.lp

import fpatterns._

trait ILPSolver {
  self: Foldables with Monoids with Readers with SimplexPhases with SimplexDomain with Numerics with Domains with Vectors with Matrices =>

  private type Cut = (Int, BigDecimal, Vector[Int, BigDecimal])
  private type Cuts = Seq[Cut]

  private def frac(n: BigDecimal) = n - scala.math.floor(n.toDouble)

  protected def deriveCut = (b: BigDecimal, row: Vector[Int, BigDecimal]) => (-frac(b), mapVector(row) { (_, v) => frac(-v) } )

  private def identifyRows = (d: Dictionary) =>
    readVectorData(readB(d)).foldLeft(List.empty[(BigDecimal, Vector[Int, BigDecimal])]) {
      (acc, e) => if (e._2.isValidInt) acc else (e._2, row(e._1, readA(d)))::acc
    }

  private def deriveCuts = identifyRows andThen  { _.map (deriveCut.tupled) }

  private def generateCuts(d: Dictionary) =
    Iterator.from(variables(d).max + 1).zip(deriveCuts(d).iterator).map { e => (e._1, e._2._1, e._2._2) }.toSeq

  private def addCut(cut: Cut) = Endo[Dictionary] { d =>
      val (index, b, row) = cut
      dictionary(addEntry(index, b).run(readB(d)), readZ0(d), readZ(d), addRow(index, row).run(readA(d)))
  }

  protected def addCuts(d: Dictionary) =
    foldableSequence[Cut].foldMap(generateCuts(d))(addCut)(endoMonoid[Dictionary]).run(d)
}
