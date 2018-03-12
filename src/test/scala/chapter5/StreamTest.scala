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

  "takeWhile" should "take no element from stream" in {
    Stream(1,2,4).takeWhile(_ => false).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Stream().takeWhile(_ => true).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Stream(1,2,4).takeWhile(_ < 2).toList shouldBe Stream(1).toList
  }

  it should "take some elements from stream" in {
    Stream(1,2,4).takeWhile(_ < 3).toList shouldBe Stream(1,2).toList
  }

  it should "take all elements from stream" in {
    Stream(1,2,4).takeWhile(_ < 5).toList shouldBe Stream(1,2,4).toList
  }

  it should "behave the same way as takeWhile2" in {
    Stream().takeWhile(_ => true).toList shouldBe Stream().takeWhile2(_ => true).toList
    Stream(1,2,4).takeWhile(_ => false).toList shouldBe Stream(1,2,4).takeWhile2(_ => false).toList
    Stream(1,2,4).takeWhile(_ < 3).toList shouldBe Stream(1,2,4).takeWhile2(_ < 3).toList
    Stream(1,2,4).takeWhile(_ < 5).toList shouldBe Stream(1,2,4).takeWhile2(_ < 5).toList
    Stream(1,2,4).takeWhile(_ < 2).toList shouldBe Stream(1,2,4).takeWhile2(_ < 2).toList
  }

  "forAll" should "return true when stream is empty" in {
    Stream().forAll(_ => false) shouldBe true
  }

  it should "return false when at least one element does not pass the predicate" in {
    Stream(1,2,3,8,8,8,8,8,8,8,8,8,8).forAll(_ < 2) shouldBe false
  }

  it should "return true when all elements pass the predicate" in {
    Stream(1,2,3,8,8,8,8,8,8,8,8,8,8).forAll(_ > 0) shouldBe true
  }

  "headOption" should "return None when stream is empty" in {
    Stream().headOption shouldBe None
  }

  it should "return Some value when stream is not empty" in {
    Stream(5,6).headOption shouldBe Some(5)
  }

  it should "behave the same way as headOption2" in {
    Stream(5,6).headOption shouldBe Stream(5,6).headOption2
    Stream().headOption shouldBe Stream().headOption2
  }

  "map" should "return an empty stream when stream is empty" in {
    Stream().map(_ => false) shouldBe Empty
  }

  it should "map over a fn when stream is not empty" in {
    Stream("foo", "bar").map(_.toUpperCase).toList shouldBe List("FOO", "BAR")
  }

  it should "behave the same as map2" in {
    Stream().map(_ => false) shouldBe Stream().map2(_ => false)
    Stream("foo", "bar").map(_.toUpperCase).toList shouldBe Stream("foo", "bar").map2(_.toUpperCase).toList
  }
}
