package math.lp

import scala.io.Source

object MainSimplex extends DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {
  def main(arg: Array[String]) {
    val ls = Source.fromFile("dict7").getLines().toStream
    val D = readDictionary(ls)
    println(format(makeOutput(D)))
  }
}

