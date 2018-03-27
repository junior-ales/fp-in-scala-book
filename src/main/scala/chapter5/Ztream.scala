package chapter5

trait Ztream[+A] {

  import chapter5.Ztream.cons

  // The arrow `=>` in front of the argument type `B` means that the function `f` takesits second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Ztream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def takeViaUnfold(n: Int): Ztream[A] =
    Ztream.unfold((this, n)) {
      case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
      case _ => None
    }

  def drop(n: Int): Ztream[A] = this match {
    case Cons(_, t) => if (n > 0) t().drop(n - 1) else this
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Ztream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Ztream[A] =
    foldRight(Empty: Ztream[A])((a, acc) => if (p(a)) cons(a, acc) else Empty)

  def takeWhileViaUnfold(p: A => Boolean): Ztream[A] = Ztream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((x, acc) => p(x) && acc)

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](fn: A => B): Ztream[B] = foldRight(Empty: Ztream[B])((a, acc) => cons(fn(a), acc))

  def map2[B](fn: A => B): Ztream[B] = this match {
    // case Cons(h, t) => Cons(() => fn(h()), () => t().map2(fn))
    case Cons(h, t) => cons(fn(h()), t().map2(fn))
    case _ => Empty
  }

  def mapViaUnfold[B](fn: A => B): Ztream[B] =
    Ztream.unfold(this)(s => s.headOption.map(fn).map(b => (b, s.drop(1))))

  def filter(fn: A => Boolean): Ztream[A] =
    foldRight(Empty: Ztream[A])((a, acc) => if (fn(a)) cons(a, acc) else acc)

  def filter2(fn: A => Boolean): Ztream[A] = this match {
    case Cons(h, t) => if (fn(h())) cons(h(), t().filter2(fn)) else t().filter2(fn)
    case _ => Empty
  }

  def append[T >: A](x: => Ztream[T]): Ztream[T] = foldRight(x)((a, b) => cons(a, b))

  def flatMap[T](fn: A => Ztream[T]): Ztream[T] =
    foldRight(Ztream.empty[T])((a, acc) => fn(a) append acc)

  def flatMap2[T](fn: A => Ztream[T]): Ztream[T] = this match {
    case Cons(h, t) => fn(h()) match {
      case Cons(h2, _) => Cons(h2, () => t().flatMap(fn))
      case _ => Empty
    }
    case _ => Empty
  }

  def zipWith[B, C](s: Ztream[B])(fn: (A, B) => C): Ztream[C] =
    Ztream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((fn(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s: Ztream[B]): Ztream[(Option[A], Option[B])] =
    Ztream.unfold((this, s)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Ztream.empty[A], t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Ztream.empty[B]))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[B](s: Ztream[B]): Boolean = zipWith(s)(_ == _) match {
    case Empty => false
    case xs@(_) => xs.forAll(identity)
  }

  def tails: Ztream[Ztream[A]] =
    Ztream.unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    } append Ztream(Ztream.empty)

  def hasSubsequence[T](s: Ztream[T]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B >: A](z: B)(fn: (A, => B) => B): Ztream[B] = Ztream.unfold(this) {
    case s@(Cons(_, t)) => Some((s.foldRight(z)(fn), t()))
    case _ => None
  } append Ztream(z)

}

case object Empty extends Ztream[Nothing]

case class Cons[+A](h: () => A, t: () => Ztream[A]) extends Ztream[A]

object Ztream {
  def cons[A](hd: => A, tl: => Ztream[A]): Ztream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Ztream[A] = Empty

  def apply[A](as: A*): Ztream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Ztream[A] = f(z) match {
    case Some((v, s)) => cons(v, unfold(s)(f))
    case _ => Empty
  }

  val ones: Ztream[Int] = cons(1, ones)

  val ones2: Ztream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant[A](a: A): Ztream[A] = cons(a, constant(a))

  def constant2[A](a: A): Ztream[A] = unfold(a)(_ => Some((a, a)))

  def from(n: Int): Ztream[Int] = cons(n, from(n + 1))

  def from2(n: Int): Ztream[Int] = unfold(n)(s => Some((s, s + 1)))

  private def innerFibs(x: Int, y: Int): Ztream[Int] = cons(x, innerFibs(y, x + y))

  def fibs(): Ztream[Int] = innerFibs(0, 1)

  def fibs2(): Ztream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

}
