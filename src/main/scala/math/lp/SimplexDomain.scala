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

  protected def basic = readA map { _.domains._1 }

  protected def nonBasic = readA map { _.domains._2 }

  protected def readA = Reader[Dictionary, Mat] { _.a }

  protected def readZ = Reader[Dictionary, Vec] { _.z }

  protected def readB = Reader[Dictionary, Vec] { _.b }

  protected def readZ0 = Reader[Dictionary, BigDecimal] { _.z0 }

  protected def variables = for {
    b <- basic
    n <- nonBasic
  } yield b ++ n

  protected def getB(b: Int) = readB map (readVectorData(_).get(b))

  protected def getA(b: Int, n: Int) = readA map (readMatrixData(_).get((b,n)))
}
