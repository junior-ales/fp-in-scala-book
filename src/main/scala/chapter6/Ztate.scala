package chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (n, r) if n == Int.MinValue => nonNegativeInt(r)
    case (n, r) if n < 0 => (n * -1, r)
    case i@(_) => i
  }

  def nonNegativeIntFromBook(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (tp, r) = intDouble(rng)

    (tp.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def innerInts(c1: Int, r1: RNG, acc: List[Int]): (List[Int], RNG) = c1 match {
      case c if c < 1 => (acc, r1)
      case _ =>
        val (i, r2) = r1.nextInt
        innerInts(c1 - 1, r2, i :: acc)
    }

    innerInts(count, rng, Nil)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class Ztate[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): Ztate[S, B] =
    ???

  def map2[B, C](sb: Ztate[S, B])(f: (A, B) => C): Ztate[S, C] =
    ???

  def flatMap[B](f: A => Ztate[S, B]): Ztate[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Ztate {
  type Rand[A] = Ztate[RNG, A]

  def simulateMachine(inputs: List[Input]): Ztate[Machine, (Int, Int)] = ???
}
