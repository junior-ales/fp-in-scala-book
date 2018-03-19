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

  it should "behave like takeViaUnfold" in {
    Stream().take(8).toList shouldBe Stream().takeViaUnfold(8).toList
    Stream(1, 2, 4).take(1).toList shouldBe Stream(1, 2, 4).takeViaUnfold(1).toList
    Stream(1, 2, 4).take(2).toList shouldBe Stream(1, 2, 4).takeViaUnfold(2).toList
    Stream(1, 2, 4).take(3).toList shouldBe Stream(1, 2, 4).takeViaUnfold(3).toList
    Stream(1, 2, 4).take(0).toList shouldBe Stream(1, 2, 4).takeViaUnfold(0).toList
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

  it should "behave the same way as takeWhileViaUnfold" in {
    Stream().takeWhile(_ => true).toList shouldBe Stream().takeWhileViaUnfold(_ => true).toList
    Stream(1, 2, 4).takeWhile(_ => false).toList shouldBe Stream(1, 2, 4).takeWhileViaUnfold(_ => false).toList
    Stream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Stream(1, 2, 4).takeWhileViaUnfold(_ < 3).toList
    Stream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Stream(1, 2, 4).takeWhileViaUnfold(_ < 5).toList
    Stream(1, 2, 4).takeWhile(_ < 2).toList shouldBe Stream(1, 2, 4).takeWhileViaUnfold(_ < 2).toList
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
    Stream.empty[Int].append(Stream(4)).toList shouldBe List(4)
    Stream(5).append(Stream("bla")).toList shouldBe List(5, "bla")
    Stream("foo", "bar", "baz").append(Stream("eita")).toList shouldBe List("foo", "bar", "baz", "eita")
  }

  "flatMap" should "map and flatten over a function" in {
    Stream().flatMap(_ => Stream(5)) shouldBe Empty
    Stream(5).flatMap(Stream(_)).toList shouldBe List(5)
    Stream(5, 8).flatMap(x => if (x == 5) Stream(x) else Empty).toList shouldBe List(5)

    Stream().flatMap(_ => Stream(5)) shouldBe Stream().flatMap2(_ => Stream(5))
    Stream(5).flatMap(Stream(_)).toList shouldBe Stream(5).flatMap2(Stream(_)).toList
    Stream(5, 8).flatMap(x => if (x == 5) Stream(x) else Empty).toList shouldBe Stream(5, 8).flatMap2(x => if (x == 5) Stream(x) else Empty).toList
  }

  "constant" should "return infinite stream of a value" in {
    Stream.constant(5).take(0).toList shouldBe Nil
    Stream.constant(5).take(13).toList shouldBe List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
  }

  it should "behave like constant2" in {
    Stream.constant(5).take(0).toList shouldBe Stream.constant2(5).take(0).toList
    Stream.constant(5).take(13).toList shouldBe Stream.constant2(5).take(13).toList
  }

  "from" should "return infinite stream of int starting at a given value" in {
    Stream.from(3).take(0).toList shouldBe Nil
    Stream.from(3).take(10).toList shouldBe List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  it should "behave like from2" in {
    Stream.from(3).take(0).toList shouldBe Stream.from2(3).take(0).toList
    Stream.from(3).take(10).toList shouldBe Stream.from2(3).take(10).toList
  }

  "fibs" should "return an empty stream when predicate finishes at first loop" in {
    Stream.fibs().take(0).toList shouldBe Nil
  }

  it should "return the first elem of fibonacci's sequence" in {
    Stream.fibs().take(1).toList shouldBe List(0)
  }

  it should "return the first two elems of fibonacci's sequence" in {
    Stream.fibs().take(2).toList shouldBe List(0, 1)
  }

  it should "return the first three elems of fibonacci's sequence" in {
    Stream.fibs().take(3).toList shouldBe List(0, 1, 1)
  }

  it should "return several elems of fibonacci's sequence" in {
    Stream.fibs().take(16).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
  }

  it should "behave like fibs2" in {
    Stream.fibs().take(0).toList shouldBe Stream.fibs2().take(0).toList
    Stream.fibs().take(1).toList shouldBe Stream.fibs2().take(1).toList
    Stream.fibs().take(2).toList shouldBe Stream.fibs2().take(2).toList
    Stream.fibs().take(3).toList shouldBe Stream.fibs2().take(3).toList
    Stream.fibs().take(16).toList shouldBe Stream.fibs2().take(16).toList
  }

  "unfold" should "produce an infinity sequence of numbers" in {
    Stream.unfold(1)(n => Some((n, n + 1))).take(4).toList shouldBe List(1, 2, 3, 4)
  }

  it should "produce a sequence of numbers up to 4" in {
    val stream = Stream.unfold(1)(n =>
      if (n > 4) None
      else Some((n, n + 1))
    )

    stream.take(50).toList shouldBe List(1, 2, 3, 4)
  }

  "ones" should "produce an infinite sequence of number 1" in {
    Stream.ones.take(3).toList shouldBe List(1, 1, 1)
    Stream.ones.take(0).toList shouldBe Nil
  }

  it should "behave the same way as ones2" in {
    Stream.ones.take(3).toList shouldBe Stream.ones2.take(3).toList
    Stream.ones.take(0).toList shouldBe Stream.ones2.take(0).toList
  }

  "zipWith" should "merge streams" in {
    Stream.empty[Int].zipWith(Stream(5, 6, 7))(_ + _).toList shouldBe Nil
    Stream(5, 6, 7).zipWith(Stream.empty[Int])(_ + _).toList shouldBe Nil
    Stream(1, 2).zipWith(Stream(5, 5))(_ + _).toList shouldBe List(6, 7)
    Stream(10, 11, 12).zipWith(Stream.ones)(_ + _).toList shouldBe List(11, 12, 13)
    Stream.ones.zipWith(Stream(10, 11, 12))(_ + _).toList shouldBe List(11, 12, 13)
  }

  "zipAll" should "merge stream until either are exhausted" in {
    Stream.empty[Int].zipAll(Stream.empty[Int]).take(10).toList shouldBe Nil
    Stream.empty[Int].zipAll(Stream.ones).take(3).toList shouldBe List((None, Some(1)), (None, Some(1)), (None, Some(1)))
    Stream.ones.zipAll(Stream.empty[Int]).take(3).toList shouldBe List((Some(1), None), (Some(1), None), (Some(1), None))

    Stream(8, 2).zipAll(Stream(7, 9)).toList shouldBe List((Some(8), Some(7)), (Some(2), Some(9)))
    Stream(8, 2).zipAll(Stream(7, 9)).take(10).toList shouldBe List((Some(8), Some(7)), (Some(2), Some(9)))

    Stream(8, 2, 6).zipAll(Stream.ones).take(4).toList shouldBe List((Some(8), Some(1)), (Some(2), Some(1)), (Some(6), Some(1)), (None, Some(1)))
  }

  "startsWith" should "tell when a stream is a prefix of another stream" in {
    Stream.empty[Int].startsWith(Stream.empty[Int]) shouldBe false
    Stream(20).startsWith(Stream.empty[Int]) shouldBe false
    Stream.empty[Int].startsWith(Stream("batman")) shouldBe false

    Stream(1, 2, 3) startsWith Stream(1, 2) shouldBe true
    Stream("eita") startsWith Stream("eita") shouldBe true
  }
}
