package math.lp

import fpatterns.{ Monoids, Reader }

trait SimplexPhases extends SimplexInitialization with SimplexPivot {
  self: Monoids with SimplexDomain with Numerics with Domains with Vectors with Matrices =>

  protected sealed trait SimplexPhase {
    def input: Dictionary
  }
  protected case class PhaseI(override val input: Dictionary) extends SimplexPhase
  protected case class PhaseII(override val input: Dictionary) extends SimplexPhase

  protected sealed trait Solution
  protected case class Feasible(steps: Int, d: Dictionary) extends Solution
  protected case class Infeasible(e: Infeasibility, d: Dictionary) extends Solution

  protected sealed trait Infeasibility
  protected object UnboundedInput extends Infeasibility
  protected object UnboundedAuxiliary extends Infeasibility

  protected def inputDict[P <: SimplexPhase] = Reader[P, Dictionary] { _.input }

  private def analyze = Reader[Dictionary, SimplexPhase] { d =>
    findValue(readB(d))(_ < 0) match {
      case Some(_) => PhaseI(d)
      case _       => PhaseII(d)
    }
  }

  private def solve(p: PhaseII): Solution = loopPivot(inputDict(p)) match {
    case (steps, Done(d))   => Feasible(steps, d)
    case (steps, Unbounded) => Infeasible(UnboundedInput, inputDict(p))
  }

  private def solve(p: PhaseI): Solution = solveAuxiliary(inputDict(p)) match {
    case (steps, Done(aux)) => solve(PhaseII(updateAuxiliaryCost(inputDict(p))(aux)))
    case (steps, Unbounded) => Infeasible(UnboundedAuxiliary, inputDict(p))
  }

  protected def solve(d: Dictionary): Solution = analyze(d) match {
    case p @ PhaseI(_)  => solve(p)
    case p @ PhaseII(_) => solve(p)
  }
}
