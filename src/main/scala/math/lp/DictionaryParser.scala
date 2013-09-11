package math.lp

import fpatterns.State
import scala._

trait DictionaryParser {
  self: Simplex with Numerics with Matrices with Vectors with Domains =>

  protected def readDictionary = (for {
    pair <- readRowCol
    ms <- getIndices
    ns <- getIndices
    bs <- getB(ms)
    as <- getA(ms, ns)
    z0 <- getZ0
    cn <- getCn(ns)
  } yield dictionary(bs, z0, cn, matrix((ms, ns), as))) andThen (_._2)

  private def readRowCol = State[Stream[String], (Int, Int)] { s =>
    val rc = s.take(1).head.split(" ").filterNot(_.isEmpty) map (_.toInt)
    (s.drop(1), (rc(0), rc(1)))
  }

  private def getIndices = State[Stream[String], Domain[Int]] { s =>
    (s.drop(1), s.take(1).head.split(" ").filterNot(_.isEmpty).map(_.toInt).toSet)
  }

  private def getB(m: Domain[Int]) = State[Stream[String], Vec] { s =>
    val bs = s.take(1).head.split(" ").filterNot(_.isEmpty).map(BigDecimal(_))
    (s.drop(1), sparseVector(m, m.toSeq.sorted.zip(bs).toMap))
  }

  private def getA(m: Domain[Int], n: Domain[Int]) = State[Stream[String], Data[Int, Int, BigDecimal]] { s =>
    val ms = m.toSeq.sorted
    val ns = n.toSeq.sorted
    val (_, map) = s.take(ms.size).foldLeft((0, Map.empty[(Int, Int), BigDecimal])) { (current, coeffs) =>
      val keys = ns map (j => (ms(current._1), j))
      (current._1 + 1, current._2 ++ keys.zip(coeffs.split(" ").filterNot(_.isEmpty).map(BigDecimal(_))).toMap)
    }
    (s.drop(ms.size), map)
  }

  private def getZ0 = State[Stream[String], BigDecimal] { s =>
    (s, BigDecimal(s.head.split(" ")(0)))
  }

  private def getCn(n: Domain[Int]) = State[Stream[String], Vec] { s =>
    val cns = s.head.split(" ").tail.toSeq.map(_.trim).filterNot(_.isEmpty).map(BigDecimal(_))
    val v = sparseVector(n, n.toSeq.sorted.zip(cns).toMap)
    (s, v)
  }
}
