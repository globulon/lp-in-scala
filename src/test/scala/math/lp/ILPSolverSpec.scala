package math.lp

import org.scalatest.{Inspectors, Matchers, WordSpec}
import fpatterns._
import scala.io.Source
import java.nio.file.Paths

final class ILPSolverSpec
  extends WordSpec
  with Matchers with Inspectors
  with DictionaryIO
  with SimplexPhases
  with ILPSolver
  with Foldables
  with Monoids
  with Readers
  with SimplexDomain
  with Numerics
  with Domains
  with Vectors
  with Matrices {

  "solver" must {
    "derive simple cut " in {
      val cut = deriveCut(BigDecimal(1.5), sparseVector[Int, BigDecimal](domain(3,4), Map(3 -> BigDecimal(-0.25), 4 -> BigDecimal(-0.25))))
     cut._1 should be (BigDecimal(-0.5))
      readDomain(cut._2) should be (Set(3,4))
      readVectorData(cut._2) should be (Map(3 -> BigDecimal(0.25), 4 -> BigDecimal(0.25)))
    }

    "derive more complex cut" in {
      val cut = deriveCut(BigDecimal(2.0/3), sparseVector[Int, BigDecimal](domain(5,4), Map(5 -> BigDecimal(-2.0/3), 4 -> BigDecimal(1.0/3))))
      cut._1 should be (BigDecimal(-2.0/3))
      readDomain(cut._2) should be (Set(5,4))
      readVectorData(cut._2).get(5).get should be (BigDecimal(2.0/3) +- 0.001)
      readVectorData(cut._2).get(4).get should be (BigDecimal(2.0/3) +- 0.001)
    }

    "add cuts in ilpTest1" in {
      withDictionary("unitTests", "ilpTest1") { d =>
        assertFeasible(solve(d)) { r =>
          variables(addCuts(r)) should be (Range.inclusive(1,5).toSet)
          expect(getB(5)(addCuts(r))) { _ should be (BigDecimal(-0.5)) }
          expect(getA(5,3)(addCuts(r))) { _ should be (BigDecimal(0.25)) }
          expect(getA(5,4)(addCuts(r))) { _ should be (BigDecimal(0.25))}
        }
      }
    }

    "add cuts in ilpTest2" in {
      withDictionary("unitTests", "ilpTest2") { d =>
        assertFeasible(solve(d)) { r =>
          variables(addCuts(r)) should be (Range.inclusive(1,13).toSet)

          expect(getB(10)(addCuts(r))) { _ should be (BigDecimal(-1.0/3) +- 0.001) }
          expect(getB(11)(addCuts(r))) { _ should be (BigDecimal(-1.0/3) +- 0.001) }
          expect(getB(12)(addCuts(r))) { _ should be (BigDecimal(-2.0/3) +- 0.001) }
          expect(getB(13)(addCuts(r))) { _ should be (BigDecimal(-2.0/3) +- 0.001) }

          expect(getA(10,8)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }
          expect(getA(10,9)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }
          expect(getA(10,3)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }

          expect(getA(11,8)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }
          expect(getA(11,9)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }
          expect(getA(11,3)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }

          expect(getA(12,8)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }
          expect(getA(12,9)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }
          expect(getA(12,3)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }

          expect(getA(13,8)(addCuts(r))) { _ should be (BigDecimal(1.0/3) +- 0.001) }
          expect(getA(13,9)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }
          expect(getA(13,3)(addCuts(r))) { _ should be (BigDecimal(2.0/3) +- 0.001) }

        }
      }
    }

  }

  private def expect[A](ma: Option[A])(f: A => Unit) = {
    ma should not be 'empty
    ma foreach { f }
  }

  private def assertFeasible(s: Solution)(f: Dictionary => Unit) = s match {
    case Feasible(_, d) => f(d)
    case _ => fail("dictionary should be feasible")
  }

  private def withDictionary(prefix: String, name: String)(f: Dictionary => Unit) =
    closing(Source.fromFile(location(prefix, name))) { source =>
      val ls = source.getLines().toStream
      f(readDictionary(ls))
    }

  private def location(prefix: String, name: String) = Paths.get("src", "test", "resources", "part4", prefix, name).toUri

}
