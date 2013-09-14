package math.lp

import scala.io.Source

object MainSimplex extends DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {
  def main(arg: Array[String]) {
//    Range(1,11) foreach { i =>
//      println(s"===============$i====================")
//      val ls = Source.fromFile(s"dict$i").getLines().toStream
//      val D = readDictionary(ls)
//      println(format(makeOutput(D)))
//    }
//
//    Range(1,6) foreach { i =>
//      println(s"===============$i====================")
//
//      val ls = Source.fromFile(s"part$i.dict").getLines().toStream
//      val D = readDictionary(ls)
//      println(format(makeOutput(D)))
//    }

      Range(1,6) foreach { i =>
        println(s"===============$i====================")
        val ls  = Source.fromFile(s"part$i.dict").getLines().toStream
        val P = loopPivot(readDictionary(ls))
        P match {
          case (s, Done(r)) => println(s"${r}\n${s}")
          case _ => "UNBOUNDED"
        }
      }
  }
}

