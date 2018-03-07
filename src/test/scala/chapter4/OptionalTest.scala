package chapter4

import org.scalatest.{FlatSpec, Matchers}

class OptionalTest extends FlatSpec with Matchers {

  "map" should "Some map over a function" in {
    Some(3).map(_ + 1) shouldBe Some(4)
  }

  it should "ignore the fn in a None" in {
    None.map(_.toString) shouldBe None
  }

  "getOrElse" should "None get default" in {
    None.getOrElse(5) shouldBe 5
  }

  it should "Some get the value and ignore default" in {
    Some(42).getOrElse(5) shouldBe 42
  }

  "flatMap" should "None ignore fn" in {
    None.flatMap(_ => Some(5)) shouldBe None
  }

  it should "Some flat map over fn that returns some" in {
    Some(5).flatMap(x => Some(x + 1)) shouldBe Some(6)
  }

  it should "Some flat map over fn that returns none" in {
    Some("bla").flatMap(_ => None) shouldBe None
  }

  "orElse" should "None get default" in {
    None.orElse(None) shouldBe None
    None.orElse(Some(9)) shouldBe Some(9)
  }

  it should "Some get the exact same value" in {
    Some("foo").orElse(Some(6)) shouldBe Some("foo")
  }

  "filter" should "None ignore fn" in {
    None.filter(_ => true) shouldBe None
  }

  it should "Some return same value if passes predicate" in {
    Some(42).filter(_ > 0) shouldBe Some(42)
  }

  it should "Some return None if value does not pass predicate" in {
    Some(List(1, 2, 4)).filter(_.isEmpty) shouldBe None
  }

  "mean" should "None when list is empty" in {
    Optional.mean(Nil) shouldBe None
  }

  it should "calculate the mean" in {
    Optional.mean(List(600, 470, 170, 430, 300)) shouldBe Some(394)
  }

  "variance" should "None when list is empty" in {
    Optional.variance(Nil) shouldBe None
  }

  it should "Some calculate the variance" in {
    Optional.variance(List(600, 470, 170, 430, 300)) shouldBe Some(21704)
  }

  "map2" should "None when first optional is none" in {
    Optional.map2(None: Optional[Int], Some(5))(_ + _) shouldBe None
  }

  it should "None when second optional is none" in {
    Optional.map2(Some(5), None: Optional[Int])(_ + _) shouldBe None
  }

  it should "None when both optionals are none" in {
    Optional.map2(None: Optional[Int], None: Optional[Int])(_ + _) shouldBe None
  }

  it should "Some when both are some" in {
    Optional.map2(Some("eita"), Some(" laia"))(_ + _) shouldBe Some("eita laia")
  }

  "sequence" should "None when there is one none in the list" in {
    Optional.sequence(List(Some(9), None)) shouldBe None
  }

  it should "Some when there is no none in the list" in {
    Optional.sequence(List(Some('b'), Some('t'))) shouldBe Some(List('b', 't'))
  }

  it should "Some when list is empty" in {
    Optional.sequence(Nil) shouldBe Some(Nil)
  }

  it should "None when all are none" in {
    Optional.sequence(List(None, None, None)) shouldBe None
  }

  it should "be the same result as sequence2" in {
    Optional.sequence(List(Some(9), None)) shouldBe Optional.sequence2(List(Some(9), None))
    Optional.sequence(List(Some('b'), Some('t'))) shouldBe Optional.sequence2(List(Some('b'), Some('t')))
    Optional.sequence(Nil) shouldBe Optional.sequence2(Nil)
    Optional.sequence(List(None, None, None)) shouldBe Optional.sequence2(List(None, None, None))
  }

  "traverse" should "Some when list is empty" in {
    Optional.traverse(List.empty[Int])(_ => None) shouldBe Some(Nil)
  }

  it should "None when predicate evals to none" in {
    Optional.traverse(List(1, 2))(_ => None) shouldBe None
  }

  it should "Some when predicate evals to Some" in {
    Optional.traverse(List(1, 2))(x => Some(x)) shouldBe Some(List(1, 2))
  }
}
