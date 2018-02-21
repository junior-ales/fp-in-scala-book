package chapter2

import org.scalatest.{FlatSpec, Matchers}

class LiztTest extends FlatSpec with Matchers {

  "Lizt" should "have elements" in {
    val result = Lizt(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nill => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => Lizt.sum(Cons(h, t))
      case _ => 101
    }

    result shouldBe 3
  }

  "Lizt sum" should "sum elements of a non-empty list" in {
    Lizt.sum(Lizt(12, 3)) shouldBe 15
  }

  it should "sum and sum2 return the same value" in {
    Lizt.sum(Lizt(9, 5)) shouldBe Lizt.sum2(Lizt(9, 5))
    Lizt.sum(Lizt(1)) shouldBe Lizt.sum2(Lizt(1))
    Lizt.sum(Nill) shouldBe Lizt.sum2(Nill)
  }

  "Lizt tail" should "take it from a singleton list" in {
    Lizt.tail(Lizt(3)) shouldBe Nill
  }

  it should "take it from a list with multiple items" in {
    Lizt.tail(Lizt(1, 2, 3)) shouldBe Lizt(2, 3)
  }

  ignore should "take it from an empty list" in {
    // throw exception
    Lizt.tail(Nill) shouldBe ???
  }

  "Lizt setHead" should "set head of an empty list" in {
    Lizt.setHead(1, Nill) shouldBe Lizt(1)
  }

  it should "set head of singleton list" in {
    Lizt.setHead(1, Lizt(3)) shouldBe Lizt(1, 3)
  }

  it should "set head of list with multiple items" in {
    Lizt.setHead(1, Lizt(3)) shouldBe Lizt(1, 3)
  }

  "Lizt drop" should "drop one element from a list" in {
    Lizt.drop(1, Lizt('b', 'd', 'r')) shouldBe Lizt('d', 'r')
  }

  it should "drop zero elems from a list" in {
    Lizt.drop(0, Lizt('b', 'd', 'r')) shouldBe Lizt('b', 'd', 'r')
  }

  it should "drop all elems from a list" in {
    Lizt.drop(3, Lizt('b', 'd', 'r')) shouldBe Nill
  }

  it should "zero elements from an empty list" in {
    Lizt.drop(0, Nill) shouldBe Nill
  }

  ignore should "drop more than the quantity of elems in a list" in {
    // throw exception
    Lizt.drop(3, Lizt('d', 'r')) shouldBe ???
  }

  ignore should "drop one element from an empty list" in {
    // throw exception cause tail of Nill throws exception
    Lizt.drop(1, Nill) shouldBe ???
  }

  "Lizt dropWhile" should "drop no element from a non-empty list" in {
    val result =  Lizt.dropWhile(Lizt("foo", "bar"))(_ => false)
    result shouldBe Lizt("foo", "bar")
  }

  it should "not drop anything from an empty list when fn evals to true" in {
    val result = Lizt.dropWhile(Nill)(_ => true)
    result shouldBe Nill
  }

  it should "not drop anything from an empty list when fn evals to false" in {
    val result = Lizt.dropWhile(Nill)(_ => false)
    result shouldBe Nill
  }

  it should "drop one element from a non-empty list" in {
    val result = Lizt.dropWhile(Lizt("foo", "bar"))(_ == "foo")
    result shouldBe Lizt("bar")
  }

  it should "drop all elements from a non-empty list" in {
    val result = Lizt.dropWhile(Lizt(65, 89))(_ > 50)
    result shouldBe Nill
  }

  "Lizt init" should "take it from a list with multiple items" in {
    Lizt.init(Lizt(1, 2, 3)) shouldBe Lizt(1, 2)
  }

  it should "take it from a singleton list" in {
    Lizt.init(Lizt(3)) shouldBe Nill
  }

  ignore should "take it from an empty list" in {
    // throw exception
    Lizt.init(Nill) shouldBe ???
  }

  "Lizt length" should "get the length of a non-empty list" in {
    Lizt.length(Lizt('3', 'b', 'f', '1')) shouldBe 4
  }

  it should "get the length of an empty list" in {
    Lizt.length(Nill) shouldBe 0
  }

  it should "get the length of a singleton list" in {
    Lizt.length(Lizt(8)) shouldBe 1
  }

  it should "be the same as length2" in {
    Lizt.length(Nill) shouldBe Lizt.length2(Nill)
    Lizt.length(Lizt(89)) shouldBe Lizt.length2(Lizt(89))
    Lizt.length(Lizt(4,265,6,5462,4562,4)) shouldBe Lizt.length2(Lizt(4,265,6,5462,4562,4))
  }
}
