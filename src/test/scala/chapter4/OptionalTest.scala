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
    Some(List(1,2,4)).filter(_.isEmpty) shouldBe None
  }
}
