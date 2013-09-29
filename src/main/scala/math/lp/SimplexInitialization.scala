package math.lp

import fpatterns.Reader

trait SimplexInitialization {
  self: SimplexPivot with SimplexDomain with Numerics with Domains with Vectors with Matrices =>

  private def readAuxiliaryB = Reader[Dictionary, Vec] { _.b }

  private def readAuxiliaryZ0 = Reader[Dictionary, BigDecimal] { _ => 0 }

  private def readAuxiliaryZs = nonBasic map { b => sparseVector[Int, BigDecimal](b + 0, Map(0 -> -1)) }

  private def readAuxiliaryA = readA map { a =>
    val entries: Data[Int, Int, BigDecimal] = a.domains._1.foldLeft(a.entries.toList) { (acc, cur) =>
      ((cur, 0) -> BigDecimal(1)) :: acc
    }.toMap
    matrix[Int, Int, BigDecimal]((a.domains._1, a.domains._2 + 0), entries.toMap)
  }

  private def makeAuxiliary = for {
    bs <- readAuxiliaryB
    z0 <- readAuxiliaryZ0
    zs <- readAuxiliaryZs
    as <- readAuxiliaryA
  } yield dictionary(bs, z0, zs, as)

  private def readAuxiliaryLeaving = readB map {
    _.entries.foldLeft((Int.MaxValue, BigDecimal(0))) { (found, cur) =>
      if (cur._2 < found._2) cur else found
    }._1
  }

  protected def solveAuxiliary(d: Dictionary) =
    pivot(readAuxiliaryLeaving(d), 0, makeAuxiliary(d)) match {
      case Cont(aux) => loopPivot(aux)
    }
}
