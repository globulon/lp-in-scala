package math.lp


trait SimplexPhases extends SimplexInitialization with SimplexPivot {
  self: SimplexDomain  with Numerics with Domains with Vectors with Matrices =>

  protected sealed trait SimplexPhase
  protected case class Initialization(d: Dictionary)
  protected case class Resolution(d: Dictionary)

//  private def getStrategy = Reader[Dictionary, SimplexPhase]
//
//    readB map {
//    findValue(_)(_ < 0) match {
//      case Some(_) => Initialization(d)
//    }
//  }


}
