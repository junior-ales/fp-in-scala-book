package chapter6

import chapter6.Machine.MachineResult

sealed trait Input

case object Coin extends Input

case object Turn extends Input

sealed trait Output {
  def quantity: Int

  def inc(): Output

  def dec(): Output
}

case class Candies(override val quantity: Int) extends Output {
  def inc(): Candies = Candies(quantity + 1)

  def dec(): Candies = Candies(quantity - 1)
}

case class InsertedCoins(override val quantity: Int) extends Output {
  def inc(): InsertedCoins = InsertedCoins(quantity + 1)

  def dec(): InsertedCoins = InsertedCoins(quantity - 1)
}

sealed trait MachineState

case object Locked extends MachineState

case object Unlocked extends MachineState

case class Machine(locked: MachineState, candies: Candies, coins: InsertedCoins) {
  def result: MachineResult = (this.candies, this.coins)

  def unlock(): Machine = this.locked match {
    case Locked => Machine(Unlocked, this.candies, this.coins)
    case _ => this
  }
}

object Machine {
  type MachineResult = (Candies, InsertedCoins)

  private val noopZtate: Ztate[Machine, MachineResult] =
    Ztate(machine => ((machine.candies, machine.coins), machine))

  private def addCoin(): Ztate[Machine, MachineResult] = ???

  // List[Input] -> ss List[Ztate[Machine, MachineResult]] -> sequence(ss) -> Ztate[Machine, MachineResult]
  def simulateMachine(inputs: List[Input]): Ztate[Machine, MachineResult] = {
    val x =
      inputs.map {
        case Coin => Ztate((machine: Machine) => {
          val newMachine = Machine(Unlocked, machine.candies, machine.coins.inc())
          (newMachine.result, newMachine)
        })
        case Turn => ???
      }
  }
}

