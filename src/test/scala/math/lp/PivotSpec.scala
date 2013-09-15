package math.lp

import org.scalatest.{ WordSpec, Matchers }
import scala.io.Source
import fpatterns._
import java.nio.file.Paths

final class PivotSpec extends WordSpec
    with Matchers with DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {
  "Pivot serial 2" must {
    "solve input dictionary 1" in {
      getPivotInfo("part1.dict") { (e,l,r) =>
        e should be (Some(4))
        l should be (Some(16))
        r map (_.toDouble) should be (Some(1.625))
      }
    }

    "solve input dictionary 2" in {
      getPivotInfo("part2.dict") { (e,l,r) =>
        e should be (Some(2))
        l should be (Some(22))
        r map (_.toDouble) should be (Some(0.8659574468085106))
      }
    }

    "solve input dictionary 3" in {
      getPivotInfo("part3.dict") { (e,l,r) =>
        e should be (Some(1))
        l should be (Some(15))
        r map (_.toDouble) should be (Some(0.6153846153846154))
      }
    }

    "solve input dictionary 4" in {
      getPivotInfo("part4.dict") { (e,l,r) =>
        e should be (Some(2))
        l should be (Some(42))
        r map (_.toDouble) should be (Some(0.29767441860465116))
      }
    }

    "solve input dictionary 5" in {
      getPivotInfo("part5.dict") { (e,l,r) =>
        e should be (Some(3))
        l should be (Some(31))
        r map (_.toDouble) should be (Some(0.7205882352941176))
      }
    }

  }

  private def getPivotInfo[A](name: String)(f: (Option[Int], Option[Int], Option[BigDecimal]) => A) =
    closing(Source.fromFile(Paths.get("src", "test", "resources", "week2", "part1", name).toUri)) { source =>
      val d = readDictionary(source.getLines().toStream)
      val e = selectEnteringVar(d)
      val l = e flatMap (selectLeavingVar(_, d))
      val r  = for {
        ev <- e
        lv <- l
      } yield nextz0(ev, lv, d)

      f(e,l, r)
    }
}
