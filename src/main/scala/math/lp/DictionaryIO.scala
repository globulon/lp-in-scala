package math.lp

import fpatterns.State
import scala._

trait DictionaryInput {
  self: Simplex with Numerics with Matrices with Vectors with Domains =>

  protected def readDictionary = (for {
    pair <- readRowCol
    ms <- getIndices
    ns <- getIndices
    bs <- getB(ms)
    as <- getA(ms, ns)
    z0 <- getZ0
    cn <- getCn(ns)
  } yield dictionary(bs, z0, cn, matrix((ms.toSet, ns.toSet), as))) andThen (_._2)

  private def readRowCol = State[Stream[String], (Int, Int)] { s =>
    val rc = s.take(1).head.split(" ").filterNot(_.isEmpty) map (_.toInt)
    (s.drop(1), (rc(0), rc(1)))
  }

  private def getIndices = State[Stream[String], Seq[Int]] { s =>
    (s.drop(1), s.take(1).head.split(" ").filterNot(_.isEmpty).map(_.toInt))
  }

  private def getB(m: Seq[Int]) = State[Stream[String], Vec] { s =>
    val bs = s.take(1).head.split(" ").filterNot(_.isEmpty).map(BigDecimal(_))
    (s.drop(1), sparseVector(m.toSet, m.zip(bs).toMap))
  }

  private def getA(m: Seq[Int], n: Seq[Int]) = State[Stream[String], Data[Int, Int, BigDecimal]] { s =>
    val (_, map) = s.take(m.size).foldLeft((0, Map.empty[(Int, Int), BigDecimal])) { (current, coeffs) =>
      val keys = n map (j => (m(current._1), j))
      (current._1 + 1, current._2 ++ keys.zip(coeffs.split(" ").filterNot(_.isEmpty).map(BigDecimal(_))).toMap)
    }
    (s.drop(m.size), map)
  }

  private def getZ0 = State[Stream[String], BigDecimal] { s =>
    (s, BigDecimal(s.head.split(" ")(0)))
  }

  private def getCn(n: Seq[Int]) = State[Stream[String], Vec] { s =>
    val cns = s.head.split(" ").tail.toSeq.map(_.trim).filterNot(_.isEmpty).map(BigDecimal(_))
    val v = sparseVector(n.toSet, n.zip(cns).toMap)
    (s, v)
  }
}

trait DictionaryOutput {
  self: Simplex with Numerics with Matrices with Vectors with Domains =>
  protected type Output = Either[String, (Int, Int, BigDecimal)]

  protected def format: (Output) => String = {
    case Left(s) => s
    case Right(t) =>  s"${t._1}\n${t._2}\n${t._3.toDouble}"
  }

  protected def makeOutput(d: Dictionary) = (for {
    e <- selectEnteringVar(d)
    l <- selectLeavingVar(e, d)
  } yield (e, l, nextz0(e, l, d))).toRight("UNBOUNDED")
}


trait DictionaryIO extends DictionaryInput with DictionaryOutput {
  self: Simplex with Numerics with Matrices with Vectors with Domains =>

}