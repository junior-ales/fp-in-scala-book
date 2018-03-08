package chapter4

sealed trait Eider[+E,+A] {
  def map[B](f: A => B): Eider[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Eider[EE, B]): Eider[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Eider[EE, B]): Eider[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Eider[EE, B])(f: (A, B) => C): Eider[EE, C] = (this, b) match {
    case (Right(v1), Right(v2)) => Right(f(v1, v2))
    case (Left(v), _) => Left(v)
    case (_, Left(v)) => Left(v)
  }

  def map2_2[EE >: E, B, C](b: Eider[EE, B])(f: (A, B) => C): Eider[EE, C] =
    for { x <- this; y <- b } yield f(x, y)

}

case class Left[+E](get: E) extends Eider[E,Nothing]
case class Right[+A](get: A) extends Eider[Nothing,A]

object Eider {
  def traverse[E,A,B](xs: List[A])(f: A => Eider[E, B]): Eider[E, List[B]] = xs match {
    case Nil => Right(Nil)
    case h :: t => f(h).flatMap(x => traverse(t)(f).map(x :: _))
  }

  def sequence[E,A](xs: List[Eider[E,A]]): Eider[E,List[A]] = traverse(xs)(identity)

  def mean(xs: IndexedSeq[Double]): Eider[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Eider[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Eider[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
