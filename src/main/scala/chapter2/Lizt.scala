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

  def dropWhile[A](ls: Lizt[A])(fn: A => Boolean): Lizt[A] = ls match {
    case Cons(x, xs) if fn(x) => dropWhile(xs)(fn)
    case _ => ls
  }

  def init[A](ls: Lizt[A]): Lizt[A] = ls match {
    case Nill => throw new UnsupportedOperationException("Nill.init is not allowed")
    case Cons(_, Nill) => Nill
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B = as match {
    case Nill => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: Lizt[Int]): Int = foldRight(ints, 0)(_ + _)

  def product2(ds: Lizt[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def length[A](as: Lizt[A]): Int = as match {
    case Nill => 0
    case Cons(_, ts) => 1 + length(ts)
  }

  def length2[A](as: Lizt[A]): Int = foldRight(as, 0)((_, a) => 1 + a)

  def foldLeft[A,B](as: Lizt[A], z: B)(f: (B, A) => B): B = as match {
    case Nill => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ints: Lizt[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(ds: Lizt[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length3[A](as: Lizt[A]): Int = foldLeft(as, 0)((a, _) => 1 + a)

  def reverse[T](xs: Lizt[T]): Lizt[T] =
    foldLeft(xs, Nill: Lizt[T])((ys, y) => Cons(y, ys))
}
