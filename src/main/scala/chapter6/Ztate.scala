package chapter6

import chapter5.Ztream

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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
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

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

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

  def intsViaUnfold(count: Int)(rng: RNG): (List[Int], RNG) =
    Ztream.unfold(rng.nextInt)(s => Some((s, s._2.nextInt)))
      .take(count)
      .foldRight((List.empty[Int], rng))(
        (t, acc) => (acc._1 ::: List(t._1), t._2)
      )

  def intsViaUnfoldFoldLeft(count: Int)(rng: RNG): (List[Int], RNG) =
    Ztream.unfold(rng.nextInt)(s => Some((s, s._2.nextInt)))
      .take(count)
      .toList
      .foldLeft((List.empty[Int], rng))(
        (acc, t) => (t._1 :: acc._1, t._2)
      )

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)

      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val intDoubleViaBoth: Rand[(Int, Double)] = both(int, double)

  val doubleIntViaBoth: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((rng, acc) => map2(rng, acc)(_ :: _))

  def flatMap[A, B](s: Rand[A])(fn: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, rng2) = s(rng)
      fn(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng2) else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i =>
      rng => {
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          (mod, rng)
        else
          nonNegativeLessThan(n)(rng)
      })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(rng => s(rng))(a => rng => (f(a), rng))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(fn: (A, B) => C): Rand[C] =
    flatMap(ra)(
      a =>
        rng1 => {
          val (b, rng2) = rb(rng1)
          (fn(a, b), rng2)
        })
}

case class Ztate[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): Ztate[S, B] =
    flatMap(a => Ztate.unit(f(a)))

  def map2[B, C](sb: Ztate[S, B])(f: (A, B) => C): Ztate[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => Ztate[S, B]): Ztate[S, B] = Ztate(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Ztate {
  type Rand[A] = Ztate[RNG, A]

  def unit[S, A](a: A): Ztate[S, A] =
    Ztate(s => (a, s))

  def sequence[S, A](ztates: List[Ztate[S, A]]): Ztate[S, List[A]] =
    ztates.reverse.foldLeft(unit[S, List[A]](List.empty[A]))((acc, f) => f.map2(acc)(_ :: _))

  def simulateMachine(inputs: List[Input]): Ztate[Machine, (Int, Int)] = ???
}

