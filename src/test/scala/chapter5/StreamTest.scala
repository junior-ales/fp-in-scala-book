package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  "toList" should "turn an empty stream in an empty list" in {
    Stream().toList shouldBe Nil
  }

  it should "turn a non-empty stream in a non-empty list" in {
    Stream('b', '4').toList shouldBe List('b', '4')
  }

  "take" should "take no element from stream" in {
    Stream(1,2,4).take(0).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Stream().take(8).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Stream(1,2,4).take(1).toList shouldBe Stream(1).toList
  }

  it should "take some elements from stream" in {
    Stream(1,2,4).take(2).toList shouldBe Stream(1,2).toList
  }

  it should "take all elements from stream" in {
    Stream(1,2,4).take(3).toList shouldBe Stream(1,2,4).toList
  }

  "drop" should "drop no element from stream" in {
    Stream('q', 'g', 'h').drop(0).toList shouldBe List('q', 'g', 'h')
  }

  it should "drop no element from empty stream" in {
    Stream().drop(4).toList shouldBe Nil
  }

  it should "drop one element from stream" in {
    Stream('q', 'g', 'h').drop(1).toList shouldBe List('g', 'h')
  }

  it should "drop some elements from stream" in {
    Stream('q', 'g', 'h').drop(2).toList shouldBe List('h')
  }

  it should "drop all elements from stream" in {
    Stream('q', 'g', 'h').drop(3).toList shouldBe Nil
  }
}
