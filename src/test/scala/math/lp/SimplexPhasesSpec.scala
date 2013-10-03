package math.lp

import org.scalatest.{ WordSpec, Matchers }
import scala.io.Source
import fpatterns._
import java.nio.file.Paths

final class SimplexPhasesSpec extends WordSpec
    with Matchers
    with DictionaryIO
    with SimplexPhases
    with SimplexInitialization
    with SimplexPivot
    with SimplexDomain
    with Numerics with Matrices with Vectors with Domains with Monoids {

  "Auxiliary serial 1" must {
    "solve input dictionary 1" in {
      solveAuxiliaryDict("unitTests", "idict1") { (steps, aux, d) =>
        steps should be(1)
        readZ0(aux) should be(0)
        basic(aux) should be(Set(1, 2, 6))
        nonBasic(aux) should be(Set(0, 4, 5, 7, 3))
        val result = updateAuxiliaryCost(d)(aux)
        readZ0(result) should be(-3)
        readData(readZ(result)) should be(Map(5 -> 3, 7 -> 0, 3 -> -1, 4 -> 1))
        readDomain(readZ(result)) should be(Set(3, 4, 5, 7))
        readDomains(readA(result)) should be ((Set(1,2,6), Set(3, 4, 5, 7)))
      }
    }
  }

  private def solveAuxiliaryDict[A](prefix: String, name: String)(f: (Int, Dictionary, Dictionary) => A) =
    pivoting(prefix, name) {
      case ((steps, Done(aux)), d) => f(steps, aux, d)
      case r                       => fail("Unexpected result pivoting")
    }

  private def pivoting[A](prefix: String, name: String)(f: ((Int, PivotStatus), Dictionary) => A) =
    closing(Source.fromFile(location(prefix, name))) { source =>
      val ls = source.getLines().toStream
      val d = readDictionary(ls)
      f(solveAuxiliary(d), d)
    }

  private def location(prefix: String, name: String) = Paths.get("src", "test", "resources", "week4", prefix, name).toUri

}
