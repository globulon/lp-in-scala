package math.lp

import org.scalatest.{ WordSpec, Matchers }
import scala.io.Source
import fpatterns._
import java.nio.file.Paths

final class SimplexSpec extends WordSpec
    with Matchers with DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {
  "Pivot serial 2" must {
    "solve input dictionary 1" in {
      solveDictionary("part1.dict") { (steps, r) =>
        steps should be(27)
        r.toDouble should be(15.506 +- 0.001)
      }
    }

    "solve input dictionary 2" in {
      solveDictionary("part2.dict") { (steps, r) =>
        steps should be(23)
        r.toDouble should be(9.953 +- 0.001)
      }
    }

    "solve input dictionary 3" in {
      solveDictionary("part3.dict") { (steps, r) =>
        steps should be(12)
        r.toDouble should be(11.827 +- 0.001)
      }
    }

    "solve input dictionary 4" in {
      solveDictionary("part4.dict") { (steps, r) =>
        steps should be(52)
        r.toDouble should be(54.594 +- 0.001)
      }
    }

    "solve input dictionary 5" in {
      solveDictionary("part5.dict") { (steps, r) =>
        steps should be(45)
        r.toDouble should be(49.171 +- 0.001)
      }
    }

  }

  private def solveDictionary[A](name: String)(f: (Int, BigDecimal) => A) =
    closing(Source.fromFile(Paths.get("src", "test", "resources", "week2", "part2", name).toUri)) { source =>
      val ls = source.getLines().toStream
      loopPivot(readDictionary(ls)) match {
        case (steps, Done(r)) => f(steps, r)
        case _                => fail("Unexpected result pivoting")
      }
    }
}
