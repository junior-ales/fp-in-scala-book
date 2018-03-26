package chapter6

import chapter6.RNG._
import org.scalatest.{FlatSpec, Matchers}

class ZtateTest extends FlatSpec with Matchers {

  case object MockSimpleAlwaysReturnMinValue extends RNG {
    def nextInt: (Int, RNG) = (Int.MinValue, Simple(0))
  }

  "nonNegativeInt" should "generate a non negative int from a positive seed" in {
    nonNegativeInt(Simple(22))._1 shouldBe 8464475
  }

  it should "generate a non negative int from a negative seed" in {
    nonNegativeInt(Simple(-98))._1 shouldBe 37705393
  }

  it should "generate a non negative int from Int.MaxValue as a seed" in {
    nonNegativeInt(Simple(Int.MaxValue))._1 shouldBe 1932566803
  }

  // Int.MinValue does not have a positive counter part
  // so it returns itself when multiplied by -1. This test
  // guarantees that nonNegativeInt returns something else.
  it should "generate a non negative int from Int.MinValue as nextInt" in {
    nonNegativeInt(MockSimpleAlwaysReturnMinValue)._1 shouldNot be(-2147483648)
  }

  it should "match book answer" in {
    nonNegativeInt(Simple(22))._1 shouldBe nonNegativeIntFromBook(Simple(22))._1
    nonNegativeInt(Simple(Int.MaxValue))._1 shouldBe nonNegativeIntFromBook(Simple(Int.MaxValue))._1

    // given the implementation decided by the author all the
    // negative numbers are offset by +1 to avoid the MinValue problem.
    nonNegativeIntFromBook(Simple(-98))._1 shouldBe 37705392

    nonNegativeIntFromBook(MockSimpleAlwaysReturnMinValue)._1 shouldNot be(-2147483648)
  }

}
