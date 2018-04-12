package chapter4

sealed trait Optional[+A] {
  def map[B](f: A => B): Optional[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def flatMap[B](f: A => Optional[B]): Optional[B] = this match {
    case Some(v) => f(v)
    case _ => None
  }

  def orElse[B >: A](ob: => Optional[B]): Optional[B] = this match {
    case Some(_) => this
    case _ => ob
  }

  def filter(f: A => Boolean): Optional[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
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
  //
  //   A `catch` block is just a pattern matching block like the ones we've seen.
  //   `case e: Exception` is a pattern that matches any `Exception`, and it binds
  //   this value to the identifier `e`. The match returns the value 43.
  //
  //   catch { case e: Exception => 43 }
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

  def variance(xs: Seq[Double]): Optional[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Optional[A], b: Optional[B])(f: (A, B) => C): Optional[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2PatternMatching[A, B, C](a: Optional[A], b: Optional[B])(f: (A, B) => C): Optional[C] =
    (a, b) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case (None, _) => None
      case (_, None) => None
    }

  def traverse[A, B](as: List[A])(f: A => Optional[B]): Optional[List[B]] = as match {
    case Nil => Some(Nil)
    case h :: t => traverse(t)(f).flatMap(list => f(h).map(_ :: list))
  }

  def sequence[A](as: List[Optional[A]]): Optional[List[A]] = traverse(as)(identity)

  def sequence2[A](as: List[Optional[A]]): Optional[List[A]] = as match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(x) :: xs => sequence2(xs).map(x :: _)
  }

}
