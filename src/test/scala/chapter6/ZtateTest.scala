package chapter6

import chapter6.RNG._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

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

  "double" should "generate a double" in {
    double(Simple(13))._1 shouldBe 0.002329114358872175
  }

  it should "generate a double between 0 and 1, not including 1" in {
    double(Simple(Random.nextInt()))._1 >= 0 shouldBe true
    double(Simple(Random.nextInt()))._1 < 1 shouldBe true
  }

  it should "behave like doubleViaMap" in {
    double(Simple(13))._1 shouldBe doubleViaMap(Simple(13))._1
    double(Simple(Random.nextInt()))._1 >= 0 shouldBe doubleViaMap(Simple(Random.nextInt()))._1 >= 0
    double(Simple(Random.nextInt()))._1 < 1 shouldBe doubleViaMap(Simple(Random.nextInt()))._1 < 1
  }

  "intDouble" should "generate a tuple Int Double" in {
    intDouble(Simple(8))._1 shouldBe ((3077991, 0.3025446915999055))
  }

  it should "behave like intDoubleViaBoth" in {
    intDouble(Simple(543))._1 shouldBe intDoubleViaBoth(Simple(543))._1
  }

  "doubleInt" should "generate a tuple Double Int" in {
    doubleInt(Simple(8))._1 shouldBe ((0.3025446915999055, 3077991))
  }

  ignore should "behave like doubleIntViaBoth" in {
    doubleInt(Simple(8))._1 shouldBe doubleIntViaBoth(Simple(8))._1
  }

  "double3" should "generate a 3-tuple Double Double Double" in {
    double3(Simple(432))._1 shouldBe ((0.0773982722312212, 0.4418651764281094, 0.4030963806435466))
  }

  "ints" should "generate a list of random integers" in {
    ints(4)(Simple(38))._1 shouldBe List(-1137957798, -1722522649, -954508689, 14620458)
  }

  it should "generate empty list when count is zero" in {
    ints(0)(Simple(38))._1 shouldBe Nil
  }

  it should "work in the same way of the Stream generated sequences" in {
    ints(3)(Simple(4))._1 shouldBe intsViaUnfold(3)(Simple(4))._1
    intsViaUnfold(3)(Simple(4))._1 shouldBe intsViaUnfoldFoldLeft(3)(Simple(4))._1
  }

  "nonNegativeLessThan" should "return a non negative number less than n" in {
    nonNegativeLessThan(843)(Simple(98))._1 shouldBe 531

    val n = 4278
    nonNegativeLessThan(n)(Simple(Random.nextInt()))._1 > 0 shouldBe true
    nonNegativeLessThan(n)(Simple(Random.nextInt()))._1 < n shouldBe true
  }

  it should "behave the same as nonNegativeLessThanViaFlatMap" in {
    nonNegativeLessThan(843)(Simple(98)) shouldBe nonNegativeLessThanViaFlatMap(843)(Simple(98))
  }

  "flatMap" should "flat map" in {
    val actualString = flatMap(int)(i1 =>
      r1 => {
        val (i2, r2) = r1.nextInt
        (s"number: ${i1 + i2}", r2)
      }
    )(Simple(54))._1

    actualString shouldBe "number: 2053351016"
  }

  "map" should "map over a Rand" in {
    map(nonNegativeInt)(i => s"likes: $i")(Simple(67))._1 shouldBe "likes: 25778176"
  }

  it should "behave like mapViaFlatMap" in {
    map(nonNegativeInt)(i => s"likes: $i")(Simple(67)) shouldBe mapViaFlatMap(nonNegativeInt)(i => s"likes: $i")(Simple(67))
  }

  "map2" should "combine two Rand" in {
    map2(int, double)((_, _))(Simple(5))._1 shouldBe ((1923744, 0.6883513862267137))
  }

  it should "behave like map2ViaFlatMap" in {
    map2(int, double)((_, _))(Simple(5)) shouldBe map2ViaFlatMap(int, double)((_, _))(Simple(5))
  }
}
