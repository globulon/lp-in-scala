package math.lp

import org.scalatest.{ WordSpec, Matchers }
import scala.io.Source
import fpatterns._
import java.nio.file.Paths

final class AuxiliarySpec extends WordSpec
    with Matchers with DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {

  "Auxiliary serial 1" must {
    "solve input dictionary 1" in {
      solveAuxiliaryDict("unitTests", "idict1") { (steps, d) =>
        steps should be(1)
        d.z0.toDouble should be(0)
      }
    }

    "solve input dictionary 2" in {
      solveAuxiliaryDict("unitTests","idict2") { (steps, d) =>
        steps should be(1)
        d.z0.toDouble should be(0)
      }
    }

    "solve input dictionary 3" in {
      solveAuxiliaryDict("unitTests","idict3") { (steps, d) =>
        steps should be(4)
        d.z0.toDouble should be(-1)
      }
    }

    "solve input dictionary 4" in {
      solveAuxiliaryDict("unitTests","idict4") { (steps, d) =>
        steps should be(1)
        d.z0.toDouble should be(0)
      }
    }

    "solve input dictionary 5" in {
      solveAuxiliaryDict("unitTests","idict5") { (steps, d) =>
        steps should be(0)
        d.z0.toDouble should be(-12)
      }
    }

    "solve input dictionary 6" in {
      solveAuxiliaryDict("unitTests","idict6") { (steps, d) =>
        steps should be(1)
        d.z0.toDouble should be(0)
      }
    }

    "solve input dictionary 7" in {
      solveAuxiliaryDict("unitTests","idict7") { (steps, d) =>
        steps should be(3)
        d.z0.toDouble should be(0)
      }
    }

    "solve input dictionary 8" in {
      solveAuxiliaryDict("unitTests","idict8") { (steps, d) =>
        steps should be(1)
        d.z0.toDouble should be(-2.163 +- 0.001)
      }
    }

    "solve input dictionary 9" in {
      solveAuxiliaryDict("unitTests","idict9") { (steps, d) =>
        steps should be(3)
        d.z0.toDouble should be(-2.355 +- 0.001)
      }
    }

    "solve input dictionary 10" in {
      solveAuxiliaryDict("unitTests","idict10") { (steps, d) =>
        steps should be(3)
        d.z0.toDouble should be(0.0 +- 1e-20)
      }
    }
  }

  "Auxiliary serial 2" must {
    "solve input dictionary 1" in {
      checkUnbounded("assignmentTests", "part1.dict") { steps =>
        steps should be(3)
      }
    }

    "solve input dictionary 2" in {
      solveAuxiliaryDict("assignmentTests", "part2.dict") { (steps, d) =>
        steps should be(5)
        d.z0.toDouble should be(-0.4038619851705438 +- 0.0001)
      }
    }

    "solve input dictionary 3" in {
      solveAuxiliaryDict("assignmentTests", "part3.dict") { (steps, d) =>
        steps should be(28)
        d.z0.toDouble should be(-0.7960686917611596 +- 0.0001)
      }
    }

    "solve input dictionary 4" in {
      checkUnbounded("assignmentTests", "part4.dict") { steps =>
        steps should be(5)
      }
    }

    "solve input dictionary 5" in {
      checkUnbounded("assignmentTests", "part5.dict") {  steps =>
        steps should be(39)
      }
    }

    "solve input dictionary 6" in {
      solveAuxiliaryDict("assignmentTests", "part6.dict") { (steps, d) =>
        steps should be(65)
        d.z0.toDouble should be(-2.8796271849337205 +- 0.0001)
      }
    }
  }

  private def solveAuxiliaryDict[A](prefix: String, name: String)(f: (Int, Dictionary) => A) =
    pivoting(prefix, name) {
      case (steps, Done(d)) => f(steps, d)
      case r                => fail("Unexpected result pivoting")
    }

  private def checkUnbounded[A](prefix: String, name: String)(f : Int => A) =
    pivoting(prefix, name) {
      case (steps, Unbounded) => f(steps)
      case _ => fail("expected unbounded dictionary")
    }

  private def pivoting[A](prefix: String, name: String)(f: (Int, PivotStatus) => A)=
    closing(Source.fromFile(location(prefix, name))) { source =>
      val ls = source.getLines().toStream
      f.tupled(solveAuxiliary(readDictionary(ls)))
    }

    private def location(prefix: String, name: String) = Paths.get("src", "test", "resources", "week4", prefix, name).toUri
}
