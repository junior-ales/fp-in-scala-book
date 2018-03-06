package chapter4

sealed trait Optional[+A] {
  def map[B](f: A => B): Optional[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = ???

  def flatMap[B](f: A => Optional[B]): Optional[B] = ???

  def orElse[B>:A](ob: => Optional[B]): Optional[B] = ???

  def filter(f: A => Boolean): Optional[A] = ???
}
case class Some[+A](get: A) extends Optional[A]
case object None extends Optional[Nothing]

object Optional {
  // def failingFn(i: Int): Int = {
  //   val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
  //   try {
  //     val x = 42 + 5
  //     x + y
  //   }
  //   catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  // }

  // def failingFn2(i: Int): Int = {
  //   try {
  //     val x = 42 + 5
  //     x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
  //   }
  //   catch { case e: Exception => 43 }
  // }

  def mean(xs: Seq[Double]): Optional[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Optional[Double] = ???

  def map2[A,B,C](a: Optional[A], b: Optional[B])(f: (A, B) => C): Optional[C] = ???

  def sequence[A](a: List[Optional[A]]): Optional[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Optional[B]): Optional[List[B]] = ???
}
