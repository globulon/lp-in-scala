package math.lp

import org.scalatest.{Inspectors, WordSpec, Matchers}
import scala.io.Source
import fpatterns._
import java.nio.file.Paths

final class PivotSpec extends WordSpec
    with Matchers with Inspectors with  DictionaryIO with Simplex with Numerics with Matrices with Vectors with Domains {
  "Pivot serial 1" must {
    "solve input dictionary 1" in {
      getPivotInfo("dict1") { (e,l,r) =>
        e should be (Some(4))
        l should be (Some(3))
        r map (_.toDouble) should be (Some(7))
      }
    }

    "solve input dictionary 2" in {
      getPivotInfo("dict2") { (e,l,r) =>
        e should be (Some(2))
        l should be (Some(3))
        r map (_.toDouble) should be (Some(4))
      }
    }

    "solve input dictionary 3" in {
      getPivotInfo("dict3") { (e,l,r) =>
        e should be (Some(1))
        l should be (Some(6))
        r map (_.toDouble) should be (Some(2))
      }
    }

    "solve input dictionary 4" in {
      getPivotInfo("dict4") { (e,l,r) =>
        e should be (Some(1))
        l should be (Some(5))
        r map (_.toDouble) should be (Some(3))
      }
    }

    "solve input dictionary 5" in {
      getPivotInfo("dict5") { (e,l,r) =>
        e should be (Some(3))
        l should be (Some(2))
        r map (_.toDouble) should be (Some(2))
      }
    }

    "solve input dictionary 6" in {
      closing(Source.fromFile(location("dict6"))) { source =>
        val d = readDictionary(source.getLines().toStream)
        val e = selectEnteringVar(d)
        e should be (Some(1))
        forAll(e) { selectLeavingVar(_, d) should be (None) }
      }
    }

    "solve input dictionary 7" in {
      getPivotInfo("dict7") { (e,l,r) =>
        e should be (Some(5))
        l should be (Some(2))
        r map (_.toDouble) should be (Some(6))
      }
    }

    "solve input dictionary 8" in {
      getPivotInfo("dict8") { (e,l,r) =>
        e should be (Some(2))
        l should be (Some(8))
        r map (_.toDouble) should be (Some(1.5076923076923077))
      }
    }

    "solve input dictionary 9" in {
      getPivotInfo("dict9") { (e,l,r) =>
        e should be (Some(1))
        l should be (Some(9))
        r map (_.toDouble) should be (Some(0.14358974358974358))
      }
    }

    "solve input dictionary 10" in {
      getPivotInfo("dict10") { (e,l,r) =>
        e should be (Some(2))
        l should be (Some(13))
        r map (_.toDouble) should be (Some(0.08285714285714285))
      }
    }

  }

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
    closing(Source.fromFile(location(name))) { source =>
      val d = readDictionary(source.getLines().toStream)
      val e = selectEnteringVar(d)
      val l = e flatMap (selectLeavingVar(_, d))
      val r  = for {
        ev <- e
        lv <- l
      } yield nextz0(ev, lv, d)

      f(e,l, r)
    }

  private def location(name: String) = Paths.get("src", "test", "resources", "week2", "part1", name).toUri
}
