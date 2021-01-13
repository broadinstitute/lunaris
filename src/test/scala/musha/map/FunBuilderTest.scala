package musha.map

import org.scalatest.funsuite.AnyFunSuite
import FunBuilder._

class FunBuilderTest extends AnyFunSuite{
  test("Build"){
    val sf1: String => Int = _.length
    val sf2: String => Int = _.lastIndexOf("y")
    val sf3: String => Int = _.lastIndexOf("o")
    val sf4: String => Int = _.lastIndexOf("g")
    val sf5: String => Int = _.lastIndexOf("u")
    val sf6: String => Int = _.lastIndexOf("y")
    val sf7: String => Int = _.lastIndexOf("z")
    val sf8: String => Int = _.lastIndexOf("!")
    val sf9: String => Int = _.lastIndexOf("?")
    val comp1: String => Int = T1(sf1)(2 + _)
    val comp2: String => Int = (sf1 & sf2)(_ + _)
    val comp3: String => Int = (sf1 & sf2 & sf3)(_ + _ + _)
    val comp4: String => Int = (sf1 & sf2 & sf3 & sf4)(_ + _ + _ + _)
    val comp5: String => Int = (sf1 & sf2 & sf3 & sf4 & sf5)(_ + _ + _ + _ + _)
    val comp6: String => Int = (sf1 & sf2 & sf3 & sf4 & sf5 & sf6)(_ + _ + _ + _ + _ + _)
    val comp7: String => Int = (sf1 & sf2 & sf3 & sf4 & sf5 & sf6 & sf7)(_ + _ + _ + _ + _ + _ + _)
    val comp8: String => Int = (sf1 & sf2 & sf3 & sf4 & sf5 & sf6 & sf7 & sf8)(_ + _ + _ + _ + _ + _ + _ + _)
    val comp9: String => Int =
      (sf1 & sf2 & sf3 & sf4 & sf5 & sf6 & sf7 & sf8 & sf9)(_ + _ + _ + _ + _ + _ + _ + _ + _)
  }

}
