package chapter5

import org.scalatest.{FlatSpec, Matchers}

class ZtreamTest extends FlatSpec with Matchers {

  "toList" should "turn an empty stream in an empty list" in {
    Ztream().toList shouldBe Nil
  }

  it should "turn a non-empty stream in a non-empty list" in {
    Ztream('b', '4').toList shouldBe List('b', '4')
  }

  "take" should "take no element from stream" in {
    Ztream(1, 2, 4).take(0).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Ztream().take(8).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Ztream(1, 2, 4).take(1).toList shouldBe Ztream(1).toList
  }

  it should "take some elements from stream" in {
    Ztream(1, 2, 4).take(2).toList shouldBe Ztream(1, 2).toList
  }

  it should "take all elements from stream" in {
    Ztream(1, 2, 4).take(3).toList shouldBe Ztream(1, 2, 4).toList
  }

  it should "behave like takeViaUnfold" in {
    Ztream().take(8).toList shouldBe Ztream().takeViaUnfold(8).toList
    Ztream(1, 2, 4).take(1).toList shouldBe Ztream(1, 2, 4).takeViaUnfold(1).toList
    Ztream(1, 2, 4).take(2).toList shouldBe Ztream(1, 2, 4).takeViaUnfold(2).toList
    Ztream(1, 2, 4).take(3).toList shouldBe Ztream(1, 2, 4).takeViaUnfold(3).toList
    Ztream(1, 2, 4).take(0).toList shouldBe Ztream(1, 2, 4).takeViaUnfold(0).toList
  }

  "drop" should "drop no element from stream" in {
    Ztream('q', 'g', 'h').drop(0).toList shouldBe List('q', 'g', 'h')
  }

  it should "drop no element from empty stream" in {
    Ztream().drop(4).toList shouldBe Nil
  }

  it should "drop one element from stream" in {
    Ztream('q', 'g', 'h').drop(1).toList shouldBe List('g', 'h')
  }

  it should "drop some elements from stream" in {
    Ztream('q', 'g', 'h').drop(2).toList shouldBe List('h')
  }

  it should "drop all elements from stream" in {
    Ztream('q', 'g', 'h').drop(3).toList shouldBe Nil
  }

  "takeWhile" should "take no element from stream" in {
    Ztream(1, 2, 4).takeWhile(_ => false).toList shouldBe Nil
  }

  it should "take no element from empty stream" in {
    Ztream().takeWhile2(_ => true).toList shouldBe Nil
  }

  it should "take one element from stream" in {
    Ztream(1, 2, 4).takeWhile2(_ < 2).toList shouldBe Ztream(1).toList
  }

  it should "take some elements from stream" in {
    Ztream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Ztream(1, 2).toList
  }

  it should "take all elements from stream" in {
    Ztream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Ztream(1, 2, 4).toList
  }

  it should "behave the same way as takeWhile2" in {
    Ztream().takeWhile(_ => true).toList shouldBe Ztream().takeWhile2(_ => true).toList
    Ztream(1, 2, 4).takeWhile(_ => false).toList shouldBe Ztream(1, 2, 4).takeWhile2(_ => false).toList
    Ztream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Ztream(1, 2, 4).takeWhile2(_ < 3).toList
    Ztream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Ztream(1, 2, 4).takeWhile2(_ < 5).toList
    Ztream(1, 2, 4).takeWhile(_ < 2).toList shouldBe Ztream(1, 2, 4).takeWhile2(_ < 2).toList
  }

  it should "behave the same way as takeWhileViaUnfold" in {
    Ztream().takeWhile(_ => true).toList shouldBe Ztream().takeWhileViaUnfold(_ => true).toList
    Ztream(1, 2, 4).takeWhile(_ => false).toList shouldBe Ztream(1, 2, 4).takeWhileViaUnfold(_ => false).toList
    Ztream(1, 2, 4).takeWhile(_ < 3).toList shouldBe Ztream(1, 2, 4).takeWhileViaUnfold(_ < 3).toList
    Ztream(1, 2, 4).takeWhile(_ < 5).toList shouldBe Ztream(1, 2, 4).takeWhileViaUnfold(_ < 5).toList
    Ztream(1, 2, 4).takeWhile(_ < 2).toList shouldBe Ztream(1, 2, 4).takeWhileViaUnfold(_ < 2).toList
  }

  "forAll" should "return true when stream is empty" in {
    Ztream().forAll(_ => false) shouldBe true
  }

  it should "return false when at least one element does not pass the predicate" in {
    Ztream(1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8).forAll(_ < 2) shouldBe false
  }

  it should "return true when all elements pass the predicate" in {
    Ztream(1, 2, 3, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8).forAll(_ > 0) shouldBe true
  }

  "headOption" should "return None when stream is empty" in {
    Ztream().headOption shouldBe None
  }

  it should "return Some value when stream is not empty" in {
    Ztream(5, 6).headOption shouldBe Some(5)
  }

  it should "behave the same way as headOption2" in {
    Ztream(5, 6).headOption shouldBe Ztream(5, 6).headOption2
    Ztream().headOption shouldBe Ztream().headOption2
  }

  "map" should "map of functions" in {
    Ztream().map(_ => false) shouldBe Empty
    Ztream("foo", "bar").map(_.toUpperCase).toList shouldBe List("FOO", "BAR")

    Ztream().map(_ => false) shouldBe Ztream().map2(_ => false)
    Ztream("foo", "bar").map(_.toUpperCase).toList shouldBe Ztream("foo", "bar").map2(_.toUpperCase).toList
  }

  "mapViaUnfold" should "map over fns" in {
    Ztream().mapViaUnfold(_ => false) shouldBe Empty
    Ztream("foo", "bar").mapViaUnfold(_.toUpperCase).toList shouldBe List("FOO", "BAR")
  }

  "filter" should "filter depending on predicate result" in {
    Ztream().filter(_ => true) shouldBe Empty
    Ztream(1, 2, 3).filter(_ == 2).toList shouldBe List(2)
    Ztream(1, 2, 3).filter(_ > 0).toList shouldBe List(1, 2, 3)
    Ztream(1, 2, 3).filter(_ < 0).toList shouldBe Nil

    Ztream().filter(_ => true) shouldBe Ztream().filter2(_ => true)
    Ztream(1, 2, 3).filter(_ == 2).toList shouldBe Ztream(1, 2, 3).filter2(_ == 2).toList
    Ztream(1, 2, 3).filter(_ > 0).toList shouldBe Ztream(1, 2, 3).filter2(_ > 0).toList
    Ztream(1, 2, 3).filter(_ < 0).toList shouldBe Ztream(1, 2, 3).filter2(_ < 0).toList
  }

  "append" should "append a elem in the end of a stream" in {
    Ztream.empty[Int].append(Ztream(4)).toList shouldBe List(4)
    Ztream(5).append(Ztream("bla")).toList shouldBe List(5, "bla")
    Ztream("foo", "bar", "baz").append(Ztream("eita")).toList shouldBe List("foo", "bar", "baz", "eita")
  }

  "flatMap" should "map and flatten over a function" in {
    Ztream().flatMap(_ => Ztream(5)) shouldBe Empty
    Ztream(5).flatMap(Ztream(_)).toList shouldBe List(5)
    Ztream(5, 8).flatMap(x => if (x == 5) Ztream(x) else Empty).toList shouldBe List(5)

    Ztream().flatMap(_ => Ztream(5)) shouldBe Ztream().flatMap2(_ => Ztream(5))
    Ztream(5).flatMap(Ztream(_)).toList shouldBe Ztream(5).flatMap2(Ztream(_)).toList
    Ztream(5, 8).flatMap(x => if (x == 5) Ztream(x) else Empty).toList shouldBe Ztream(5, 8).flatMap2(x => if (x == 5) Ztream(x) else Empty).toList
  }

  "constant" should "return infinite stream of a value" in {
    Ztream.constant(5).take(0).toList shouldBe Nil
    Ztream.constant(5).take(13).toList shouldBe List(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
  }

  it should "behave like constant2" in {
    Ztream.constant(5).take(0).toList shouldBe Ztream.constant2(5).take(0).toList
    Ztream.constant(5).take(13).toList shouldBe Ztream.constant2(5).take(13).toList
  }

  "from" should "return infinite stream of int starting at a given value" in {
    Ztream.from(3).take(0).toList shouldBe Nil
    Ztream.from(3).take(10).toList shouldBe List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  }

  it should "behave like from2" in {
    Ztream.from(3).take(0).toList shouldBe Ztream.from2(3).take(0).toList
    Ztream.from(3).take(10).toList shouldBe Ztream.from2(3).take(10).toList
  }

  "fibs" should "return an empty stream when predicate finishes at first loop" in {
    Ztream.fibs().take(0).toList shouldBe Nil
  }

  it should "return the first elem of fibonacci's sequence" in {
    Ztream.fibs().take(1).toList shouldBe List(0)
  }

  it should "return the first two elems of fibonacci's sequence" in {
    Ztream.fibs().take(2).toList shouldBe List(0, 1)
  }

  it should "return the first three elems of fibonacci's sequence" in {
    Ztream.fibs().take(3).toList shouldBe List(0, 1, 1)
  }

  it should "return several elems of fibonacci's sequence" in {
    Ztream.fibs().take(16).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
  }

  it should "behave like fibs2" in {
    Ztream.fibs().take(0).toList shouldBe Ztream.fibs2().take(0).toList
    Ztream.fibs().take(1).toList shouldBe Ztream.fibs2().take(1).toList
    Ztream.fibs().take(2).toList shouldBe Ztream.fibs2().take(2).toList
    Ztream.fibs().take(3).toList shouldBe Ztream.fibs2().take(3).toList
    Ztream.fibs().take(16).toList shouldBe Ztream.fibs2().take(16).toList
  }

  "unfold" should "produce an infinity sequence of numbers" in {
    Ztream.unfold(1)(n => Some((n, n + 1))).take(4).toList shouldBe List(1, 2, 3, 4)
  }

  it should "produce a sequence of numbers up to 4" in {
    val stream = Ztream.unfold(1)(n =>
      if (n > 4) None
      else Some((n, n + 1))
    )

    stream.take(50).toList shouldBe List(1, 2, 3, 4)
  }

  "ones" should "produce an infinite sequence of number 1" in {
    Ztream.ones.take(3).toList shouldBe List(1, 1, 1)
    Ztream.ones.take(0).toList shouldBe Nil
  }

  it should "behave the same way as ones2" in {
    Ztream.ones.take(3).toList shouldBe Ztream.ones2.take(3).toList
    Ztream.ones.take(0).toList shouldBe Ztream.ones2.take(0).toList
  }

  "zipWith" should "merge streams" in {
    Ztream.empty[Int].zipWith(Ztream(5, 6, 7))(_ + _).toList shouldBe Nil
    Ztream(5, 6, 7).zipWith(Ztream.empty[Int])(_ + _).toList shouldBe Nil
    Ztream(1, 2).zipWith(Ztream(5, 5))(_ + _).toList shouldBe List(6, 7)
    Ztream(10, 11, 12).zipWith(Ztream.ones)(_ + _).toList shouldBe List(11, 12, 13)
    Ztream.ones.zipWith(Ztream(10, 11, 12))(_ + _).toList shouldBe List(11, 12, 13)
  }

  "zipAll" should "merge stream until either are exhausted" in {
    Ztream.empty[Int].zipAll(Ztream.empty[Int]).take(10).toList shouldBe Nil
    Ztream.empty[Int].zipAll(Ztream.ones).take(3).toList shouldBe List((None, Some(1)), (None, Some(1)), (None, Some(1)))
    Ztream.ones.zipAll(Ztream.empty[Int]).take(3).toList shouldBe List((Some(1), None), (Some(1), None), (Some(1), None))

    Ztream(8, 2).zipAll(Ztream(7, 9)).toList shouldBe List((Some(8), Some(7)), (Some(2), Some(9)))
    Ztream(8, 2).zipAll(Ztream(7, 9)).take(10).toList shouldBe List((Some(8), Some(7)), (Some(2), Some(9)))

    Ztream(8, 2, 6).zipAll(Ztream.ones).take(4).toList shouldBe List((Some(8), Some(1)), (Some(2), Some(1)), (Some(6), Some(1)), (None, Some(1)))
  }

  "startsWith" should "tell when a stream is a prefix of another stream" in {
    Ztream.empty[Int].startsWith(Ztream.empty[Int]) shouldBe false
    Ztream(20).startsWith(Ztream.empty[Int]) shouldBe false
    Ztream.empty[Int].startsWith(Ztream("batman")) shouldBe false

    Ztream(1, 2, 3) startsWith Ztream(1, 2) shouldBe true
    Ztream("eita") startsWith Ztream("eita") shouldBe true
  }

  "tails" should "return the stream of suffixes of the input sequence" in {
    Ztream().tails.map(_.toList).toList shouldBe List(Nil)
    Ztream(1, 2, 3).tails.map(_.toList).toList shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  "scanRight" should "return a stream of intermediate results" in {
    Ztream.empty[Int].scanRight(0)(_ + _).toList shouldBe List(0)
    Ztream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }
}
