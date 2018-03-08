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

  "traverse" should "return Right when list is empty" in {
    Eider.traverse(List.empty[Int])(_ => Left(0)) shouldBe Right(Nil)
  }

  it should "return the Left when predicate results in one Left in the list" in {
    Eider.traverse(List(0,1,2))(x => if (x > 1) Left(x) else Right(x)) shouldBe Left(2)
  }

  it should "return the first Left when predicate results in more than one Left" in {
    Eider.traverse(List(1,2))(x => Left(x.toString)) shouldBe Left("1")
  }

  it should "return Right List when predicate results are all Right" in {
    Eider.traverse(List(1,2))(x => Right(x.toString)) shouldBe Right(List("1", "2"))
  }

  "sequence" should "return Right when list is empty" in {
    Eider.sequence(Nil) shouldBe Right(Nil)
  }

  it should "return Left when list contains one Left" in {
    Eider.sequence(List(Right(10), Right(3), Left('h'))) shouldBe Left('h')
  }

  it should "return the first Left when list contains more than one Left" in {
    Eider.sequence(List(Right(10), Left('b'), Right(3), Left('h'))) shouldBe Left('b')
  }

  it should "return Right List when list contains all Right" in {
    Eider.sequence(List(Right(10), Right(3))) shouldBe Right(List(10, 3))
  }
}
