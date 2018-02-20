package chapter1

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(creditCard: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(creditCard))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

case class Coffee(price: Double = 1.5)

case class Charge(creditCard: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (creditCard == other.creditCard)
      Charge(creditCard, amount + other.amount)
    else
      throw new IllegalArgumentException("Can't combine charges of different cards")
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.creditCard).values.map(_.reduce(_ combine _)).toList
}

case class CreditCard(owner: String)
