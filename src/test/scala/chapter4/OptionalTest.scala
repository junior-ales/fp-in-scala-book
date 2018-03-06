package chapter4

import org.scalatest.{FlatSpec, Matchers}

class OptionalTest extends FlatSpec with Matchers {

  "Optional Map" should "Some map over a function" in {
    Some(3).map(_ + 1) shouldBe Some(4)
  }

  it should "ignore the fn in a None" in {
    None.map(_.toString) shouldBe None
  }

}
