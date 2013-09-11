package math.lp

import scala.io.Source

object MainSimplex extends DictionaryParser with Simplex with Numerics with Matrices with Vectors with Domains {
  def main(arg: Array[String]) {
    val ls = Source.fromFile("dict2").getLines().toStream
    val D = readDictionary(ls)
    println(selectEnteringVar(D))
    println(selectLeavingVar(selectEnteringVar(D), D))
    println(updatez0(selectEnteringVar(D), selectLeavingVar(selectEnteringVar(D), D).get, D))
  }
}
