package chapter6

import chapter6.Machine.MachineResult

sealed trait Input

case object Coin extends Input

case object Turn extends Input

sealed trait Output {
  def quantity: Int
}

case class Candies(override val quantity: Int) extends Output {
  def dec(): Candies = Candies(quantity - 1)
}

case class InsertedCoins(override val quantity: Int) extends Output {
  def inc(): InsertedCoins = InsertedCoins(quantity + 1)
}

sealed trait MachineState

case object Locked extends MachineState

case object Unlocked extends MachineState

case class Machine(locked: MachineState, candies: Candies, coins: InsertedCoins) {
  def toStateResult: (MachineResult, Machine) = ((this.candies, this.coins), this)
}

object Machine {
  type MachineResult = (Candies, InsertedCoins)

  private val initialZtate: Ztate[Machine, MachineResult] = Ztate(_.toStateResult)

  def simulateMachine(inputs: List[Input]): Ztate[Machine, MachineResult] =
    inputs.foldLeft(initialZtate)((ztate, input) =>
      ztate.flatMap(currMachineResult => {
        val (candies, insertedCoins) = currMachineResult

        Ztate(machine =>
          (candies, machine.locked, input) match {
            case (Candies(0), _, _) => machine.toStateResult
            case (_, Unlocked, Coin) => machine.toStateResult
            case (_, Unlocked, Turn) => Machine(Locked, candies.dec(), insertedCoins).toStateResult
            case (_, Locked, Turn) => machine.toStateResult
            case (_, Locked, Coin) => Machine(Unlocked, candies, insertedCoins.inc()).toStateResult
          }
        )
      })
    )
}

