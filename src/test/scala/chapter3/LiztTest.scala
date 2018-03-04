package chapter3

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

  it should "sum2 and sum3 return the same value" in {
    Lizt.sum2(Lizt(9, 5)) shouldBe Lizt.sum3(Lizt(9, 5))
    Lizt.sum2(Lizt(1)) shouldBe Lizt.sum3(Lizt(1))
    Lizt.sum2(Nill) shouldBe Lizt.sum3(Nill)
  }

  "Lizt product" should "calculate the product of a non-empty list" in {
    Lizt.product(Lizt(1.0, 6.0)) shouldBe 6.0
  }

  it should "calculate the product of a non-empty list with a 0" in {
    Lizt.product(Lizt(1.0, 7.53, 0.0, 32.5)) shouldBe 0.0
  }

  it should "make product and product2 return the same value" in {
    Lizt.product(Nill) shouldBe Lizt.product2(Nill)
    Lizt.product(Lizt(5, 6)) shouldBe Lizt.product2(Lizt(5, 6))
    Lizt.product(Lizt(5, 0, 6)) shouldBe Lizt.product2(Lizt(5, 0, 6))
    Lizt.product(Lizt(65.76)) shouldBe Lizt.product2(Lizt(65.76))
  }

  it should "make product2 and product3 return the same value" in {
    Lizt.product2(Nill) shouldBe Lizt.product3(Nill)
    Lizt.product2(Lizt(5, 0, 6)) shouldBe Lizt.product3(Lizt(5, 0, 6))
    Lizt.product2(Lizt(65.76)) shouldBe Lizt.product3(Lizt(65.76))
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

  it should "be the same as length3" in {
    Lizt.length(Nill) shouldBe Lizt.length3(Nill)
    Lizt.length(Lizt(89)) shouldBe Lizt.length3(Lizt(89))
    Lizt.length(Lizt(4,265,6,5462,4562,4)) shouldBe Lizt.length3(Lizt(4,265,6,5462,4562,4))
  }

  "Lizt foldLeft" should "return default value when an empty list is passed as argument" in {
    Lizt.foldLeft(Nill, "default")(_ concat _) shouldBe "default"
  }

  it should "apply function in numbers of a non-empty list" in {
    Lizt.foldLeft(Lizt(5,7,8), 0)(_ + _) shouldBe 20
  }

  it should "apply function in a string of a singleton list" in {
    Lizt.foldLeft(Lizt("mate"), "hey, ")(_ concat _) shouldBe "hey, mate"
  }

  it should "have the same result as foldRigh" in {
    Lizt.foldRight(Lizt(34,6), 8)(_ * _) shouldBe Lizt.foldLeft(Lizt(34,6), 8)(_ * _)
  }

  "Lizt reverse" should "return a reversed list of an empty list" in {
    Lizt.reverse(Nill) shouldBe Nill
  }

  it should "reverse a singleton list" in {
    Lizt.reverse(Lizt(65)) shouldBe Lizt(65)
  }

  it should "reverse a non-empty list" in {
    Lizt.reverse(Lizt("foo", "bar", "eita")) shouldBe Lizt("eita", "bar", "foo")
  }

  "Lizt appendOne" should "append one element to an empty list" in {
    Lizt.appendOne(Nill: Lizt[Int], 3) shouldBe Lizt(3)
  }

  it should "append one element to a non-empty list of one element" in {
    Lizt.appendOne(Lizt('d'), 'i') shouldBe Lizt('d', 'i')
  }

  it should "append one element to a non-empty list of several elements" in {
    Lizt.appendOne(Lizt("foo", "bar"), "baz") shouldBe Lizt("foo", "bar", "baz")
  }

  "Lizt appendList" should "append two empty lists" in {
    Lizt.appendList(Nill, Nill) shouldBe Nill
  }

  it should "append two non-empty lists" in {
    Lizt.appendList(Lizt(1,2), Lizt(3,4)) shouldBe Lizt(1,2,3,4)
  }

  "Lizt addOne" should "add one to a non-empty list" in {
    Lizt.addOne(Lizt(5)) shouldBe Lizt(6)
    Lizt.addOne(Lizt(4,5)) shouldBe Lizt(5,6)
  }

  it should "add none in an empty list" in {
    Lizt.addOne(Nill) shouldBe Nill
  }

  "Lizt doubleToString" should "return an empty string if an empty lizt is passed" in {
    Lizt.doubleToString(Nill) shouldBe ""
  }

  it should "return a string of a lizt of doubles" in {
    Lizt.doubleToString(Lizt(1.2,5.3,9.9)) shouldBe "1.25.39.9"
  }
}
