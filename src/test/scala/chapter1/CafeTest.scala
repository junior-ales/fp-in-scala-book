package chapter1

import org.scalatest.{FlatSpec, Matchers}

class CafeTest extends FlatSpec with Matchers {

  "Coffee" should "have price" in {
    Coffee().price shouldBe 1.5
  }

  "Cafe" should "buy one coffee" in {
    val cafe = new Cafe()
    val cc = CreditCard("Alice")
    val expectedCharge = Charge(cc, 1.5)

    cafe.buyCoffee(cc) shouldBe(Coffee(), expectedCharge)
  }

  it should "buy several coffees" in {
    val cafe = new Cafe()
    val cc = CreditCard("Alice")
    val coffeeQuantity = 6
    val expectedCharge = Charge(cc, 9)

    cafe.buyCoffees(cc, coffeeQuantity) shouldBe(
      List(Coffee(), Coffee(), Coffee(), Coffee(), Coffee(), Coffee()),
      expectedCharge
    )
  }

  it should "buy one using fn to buy several" in {
    val cafe = new Cafe()
    val cc = CreditCard("Alice")
    val coffeeQuantity = 1
    val expectedCharge = Charge(cc, 1.5)

    cafe.buyCoffees(cc, coffeeQuantity) shouldBe(
      List(Coffee()),
      expectedCharge
    )
  }
}
