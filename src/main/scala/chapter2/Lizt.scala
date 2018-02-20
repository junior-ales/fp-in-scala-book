package chapter2

sealed trait Lizt[+A] // `List` data type, parameterized on a type, `A`

case object Nill extends Lizt[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: Lizt[A]) extends Lizt[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`. */

object Lizt { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: Lizt[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nill => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: Lizt[Double]): Double = ds match {
    case Nill => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): Lizt[A] = // Variadic function syntax
    if (as.isEmpty) Nill
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls: Lizt[A]): Lizt[A] = ls match {
    case Nill => throw new UnsupportedOperationException("Nill.tail is not allowed")
    case Cons(_, xs) => xs
  }

  def setHead[A](x: A, ls: Lizt[A]): Lizt[A] = ls match {
    case Nill => Cons(x, Nill)
    case _ => Cons(x, ls)
  }

  def drop[A](n: Int, ls: Lizt[A]): Lizt[A] = {
    if (n < 1) ls
    else drop(n - 1, tail(ls))
  }

  def dropWhile[A](fn: A => Boolean, ls: Lizt[A]): Lizt[A] = ls match {
    case Cons(x, xs) if fn(x) => dropWhile(fn, xs)
    case _ => ls
  }

  def init[A](ls: Lizt[A]): Lizt[A] = ls match {
    case Nill => throw new UnsupportedOperationException("Nill.init is not allowed")
    case Cons(_, Nill) => Nill
    case Cons(x, xs) => Cons(x, init(xs))
  }
}
