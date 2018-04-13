package chapter6

import chapter6.Machine.{MachineResult, simulateMachine}
import org.scalatest.{FlatSpec, Matchers}

class MachineTest extends FlatSpec with Matchers {

  "Machine" should "do nothing when no input is passed" in {
    val initialMachine = Machine(Unlocked, Candies(5), InsertedCoins(0))
    val expectedResult: (MachineResult, Machine) = (initialMachine.result, initialMachine)

    simulateMachine(List.empty[Input]).run(initialMachine) shouldBe expectedResult
  }

  it should "unlock machine machine when inserting one coin in a locked machine with candies left" in {
    val initialLockedMachine = Machine(Locked, Candies(2), InsertedCoins(7))

    val expectedMachine = Machine(Unlocked, Candies(2), InsertedCoins(8))
    val expectedResult: (MachineResult, Machine) = (expectedMachine.result, expectedMachine)

    simulateMachine(List(Coin)).run(initialLockedMachine) shouldBe expectedResult
  }

  ignore should "do nothing when inserting a coin into an unlocked machine" in {
    val initialLockedMachine = Machine(Unlocked, Candies(2), InsertedCoins(7))

    val expectedMachine = Machine(Unlocked, Candies(2), InsertedCoins(7))
    val expectedResult: (MachineResult, Machine) = (expectedMachine.result, expectedMachine)

    simulateMachine(List(Coin, Coin, Coin)).run(initialLockedMachine) shouldBe expectedResult
  }
  it should "do nothing when turning the knob on a locked machine"
}
