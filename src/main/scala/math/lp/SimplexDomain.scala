package math.lp

import fpatterns._

trait SimplexDomain {
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

  protected def base = readA map { _.domains._1 }

  protected def nonBasic = readA map { _.domains._1 }

  protected def readA = Reader[Dictionary, Mat] { _.a }

  protected def readZ = Reader[Dictionary, Vec] { _.z }

  protected def readB = Reader[Dictionary, Vec] { _.b }

  protected def readZ0 = Reader[Dictionary, BigDecimal] { _.z0 }
}
