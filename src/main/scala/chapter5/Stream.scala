package chapter5

trait Stream[+A] {

  import chapter5.Stream.cons

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) => if (n > 0) t().drop(n - 1) else this
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (p(a)) cons(a, acc) else Empty)

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](fn: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, acc) => cons(fn(a), acc))

  def map2[B](fn: A => B): Stream[B] = this match {
    // case Cons(h, t) => Cons(() => fn(h()), () => t().map2(fn))
    case Cons(h, t) => cons(fn(h()), t().map2(fn))
    case _ => Empty
  }

  def filter(fn: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, acc) => if (fn(a)) cons(a, acc) else acc)

  def filter2(fn: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (fn(h())) cons(h(), t().filter2(fn)) else t().filter2(fn)
    case _ => Empty
  }

  def append[T >: A](x: => Stream[T]): Stream[T] = foldRight(x)((a, b) => cons(a, b))

  def flatMap[T](fn: A => Stream[T]): Stream[T] =
    foldRight(Stream.empty[T])((a, acc) => fn(a) append acc)

  def flatMap2[T](fn: A => Stream[T]): Stream[T] = this match {
    case Cons(h, t) => fn(h()) match {
      case Cons(h2, _) => Cons(h2, () => t().flatMap(fn))
      case _ => Empty
    }
    case _ => Empty
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s)) => cons(v, unfold(s)(f))
    case _ => Empty
  }

  val ones: Stream[Int] = cons(1, ones)

  val ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  private def innerFibs(x: Int, y: Int): Stream[Int] = cons(x, innerFibs(y, x + y))

  def fibs(): Stream[Int] = innerFibs(0, 1)

}
