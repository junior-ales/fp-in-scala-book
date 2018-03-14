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
    Stream(1, 2, 4).take(0).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Stream().take(8).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Stream(1, 2, 4).take(1).toList shouldBe Stream(1).toList
  }

  it should "take some elements from stream" in {
    Stream(1, 2, 4).take(2).toList shouldBe Stream(1, 2).toList
  }

  it should "take all elements from stream" in {
    Stream(1, 2, 4).take(3).toList shouldBe Stream(1, 2, 4).toList
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
    Stream(1, 2, 4).takeWhile(_ => false).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Stream().takeWhile2(_ => true).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Stream(1, 2, 4).takeWhile2(_ < 2).toList shouldBe Stream(1).toList
  }

  it should "take some elements from stream" in {
    Stream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Stream(1, 2).toList
  }

  it should "take all elements from stream" in {
    Stream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Stream(1, 2, 4).toList
  }

  it should "behave the same way as takeWhile2" in {
    Stream().takeWhile(_ => true).toList shouldBe Stream().takeWhile2(_ => true).toList
    Stream(1, 2, 4).takeWhile(_ => false).toList shouldBe Stream(1, 2, 4).takeWhile2(_ => false).toList
    Stream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Stream(1, 2, 4).takeWhile2(_ < 3).toList
    Stream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Stream(1, 2, 4).takeWhile2(_ < 5).toList
    Stream(1, 2, 4).takeWhile(_ < 2).toList shouldBe Stream(1, 2, 4).takeWhile2(_ < 2).toList
  }

  "forAll" should "return true when stream is empty" in {
    Stream().forAll(_ => false) shouldBe true
  }

  it should "return false when at least one element does not pass the predicate" in {
    Stream(1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8).forAll(_ < 2) shouldBe false
  }

  it should "return true when all elements pass the predicate" in {
    Stream(1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8).forAll(_ > 0) shouldBe true
  }

  "headOption" should "return None when stream is empty" in {
    Stream().headOption shouldBe None
  }

  it should "return Some value when stream is not empty" in {
    Stream(5, 6).headOption shouldBe Some(5)
  }

  it should "behave the same way as headOption2" in {
    Stream(5, 6).headOption shouldBe Stream(5, 6).headOption2
    Stream().headOption shouldBe Stream().headOption2
  }

  "map" should "map of functions" in {
    Stream().map(_ => false) shouldBe Empty
    Stream("foo", "bar").map(_.toUpperCase).toList shouldBe List("FOO", "BAR")

    Stream().map(_ => false) shouldBe Stream().map2(_ => false)
    Stream("foo", "bar").map(_.toUpperCase).toList shouldBe Stream("foo", "bar").map2(_.toUpperCase).toList
  }

  "filter" should "filter depending on predicate result" in {
    Stream().filter(_ => true) shouldBe Empty
    Stream(1, 2, 3).filter(_ == 2).toList shouldBe List(2)
    Stream(1, 2, 3).filter(_ > 0).toList shouldBe List(1, 2, 3)
    Stream(1, 2, 3).filter(_ < 0).toList shouldBe Nil

    Stream().filter(_ => true) shouldBe Stream().filter2(_ => true)
    Stream(1, 2, 3).filter(_ == 2).toList shouldBe Stream(1, 2, 3).filter2(_ == 2).toList
    Stream(1, 2, 3).filter(_ > 0).toList shouldBe Stream(1, 2, 3).filter2(_ > 0).toList
    Stream(1, 2, 3).filter(_ < 0).toList shouldBe Stream(1, 2, 3).filter2(_ < 0).toList
  }

  "append" should "append a elem in the end of a stream" in {
    Stream().append(5).toList shouldBe List(5)
    Stream(5).append("bla").toList shouldBe List(5, "bla")
    Stream("foo", "bar", "baz").append("eita").toList shouldBe List("foo", "bar", "baz", "eita")
  }

  "flatMap" should "map and flatten over a function" in {
    Stream().flatMap(_ => Stream(5)) shouldBe Empty
    Stream(5).flatMap(Stream(_)).toList shouldBe List(5)
    Stream(5, 8).flatMap(x => if (x == 5) Stream(x) else Empty).toList shouldBe List(5)
  }
}
