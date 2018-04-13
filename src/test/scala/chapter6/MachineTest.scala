package chapter6

import chapter6.Machine.simulateMachine
import org.scalatest.{FlatSpec, Matchers}

class MachineTest extends FlatSpec with Matchers {

  "Machine" should "do nothing when no input is passed" in {
    val initialMachine = Machine(Unlocked, Candies(5), InsertedCoins(0))

    simulateMachine(List.empty[Input]).run(initialMachine) shouldBe initialMachine.toStateResult
  }

  // Rule 1
  it should "unlock machine machine when inserting one coin in a locked machine with candies left" in {
    val initialMachine = Machine(Locked, Candies(2), InsertedCoins(7))
    val expectedMachine = Machine(Unlocked, Candies(2), InsertedCoins(8))

    simulateMachine(List(Coin)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  // Rule 2
  it should "dispense a candy and become locked when turning the knob on an unlocked machine" in {
    val initialMachine = Machine(Unlocked, Candies(6), InsertedCoins(21))
    val expectedMachine = Machine(Locked, Candies(5), InsertedCoins(21))

    simulateMachine(List(Turn)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  // Rule 3a
  it should "do nothing when turning the knob on a locked machine" in {
    val initialMachine = Machine(Locked, Candies(34), InsertedCoins(16))
    val expectedMachine = initialMachine.copy()

    simulateMachine(List(Turn, Turn)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  // Rule 3b
  it should "do nothing when inserting a coin into an unlocked machine" in {
    val initialMachine = Machine(Unlocked, Candies(2), InsertedCoins(7))
    val expectedMachine = Machine(Unlocked, Candies(2), InsertedCoins(7))

    simulateMachine(List(Coin, Coin)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  // Rule 4
  it should "do nothing when machine is out of candies" in {
    val initialMachine = Machine(Locked, Candies(0), InsertedCoins(3))
    val expectedMachine = initialMachine.copy()

    simulateMachine(List(Coin, Turn, Turn, Coin, Coin)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  it should "successfully buy one candy - exact moves" in {
    val initialMachine = Machine(Locked, Candies(5), InsertedCoins(10))
    val expectedMachine = Machine(Locked, Candies(4), InsertedCoins(11))

    simulateMachine(List(Coin, Turn)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  it should "successfully buy one candy - with some noop moves" in {
    val initialMachine = Machine(Locked, Candies(5), InsertedCoins(10))
    val expectedMachine = Machine(Locked, Candies(4), InsertedCoins(11))

    simulateMachine(List(Turn, Coin, Coin, Turn, Turn)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }

  it should "successfully buy four candies" in {
    val initialMachine = Machine(Locked, Candies(5), InsertedCoins(10))
    val expectedMachine = Machine(Locked, Candies(1), InsertedCoins(14))

    simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(initialMachine) shouldBe expectedMachine.toStateResult
  }
}
