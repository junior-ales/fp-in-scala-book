package chapter4

import org.scalatest.{FlatSpec, Matchers}

class EiderTest extends FlatSpec with Matchers {

  "map" should "Right map over a function" in {
    Right(7).map(_ + 1) shouldBe Right(8)
  }

  it should "ignore the fn in a Left" in {
    Left(7).map(_.toString) shouldBe Left(7)
  }

  "flatMap" should "Left ignore fn" in {
    Left(41).flatMap(_ => Right('b')) shouldBe Left(41)
  }

  it should "Right flat map over fn that returns Right" in {
    Right(41).flatMap(x => Right(x + 1)) shouldBe Right(42)
  }

  it should "Right flat map over fn that returns Left" in {
    Right('b').flatMap(_ => Left(5)) shouldBe Left(5)
  }

  "orElse" should "Left get default" in {
    Left('i').orElse(Right("boo")) shouldBe Right("boo")
    Left('i').orElse(Left(5)) shouldBe Left(5)
  }

  it should "Right get the exact same value" in {
    Right(1).orElse(Right("eita")) shouldBe Right(1)
    Right(1).orElse(Left(8)) shouldBe Right(1)
  }

  "map2" should "Left when first eider is Left" in {
    Left(41).map2(Right(1))((_, _) => 0) shouldBe Left(41)
  }

  it should "Left when second eider is Left" in {
    Right(41).map2(Left(10))((_, _) => 0) shouldBe Left(10)
  }

  it should "Left when both eiders are Left" in {
    Left(41).map2(Left(10))((_, _) => 0) shouldBe Left(41)
  }

  it should "Right when both eiders are Right" in {
    Right(41).map2(Right(1))(_ + _) shouldBe Right(42)
  }

  it should "have the same result as map2_2" in {
    Left(41).map2(Right(1))((_, _) => 0) shouldBe Left(41).map2_2(Right(1))((_, _) => 0)
    Left(41).map2(Left(10))((_, _) => 0) shouldBe Left(41).map2_2(Left(10))((_, _) => 0)
    Right(41).map2(Left(10))((_, _) => 0) shouldBe Right(41).map2_2(Left(10))((_, _) => 0)
    Right(41).map2(Right(1))(_ + _) shouldBe Right(41).map2_2(Right(1))(_ + _)
  }

}
