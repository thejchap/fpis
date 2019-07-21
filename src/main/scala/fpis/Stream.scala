package fpis

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case SCons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): List[A] = (n, this) match {
    case (0, _) | (_, Empty) => List()
    case (m, SCons(h, t)) => List(h()) ++ t().take(n - 1)
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) SCons(() => a, () => b) else b)

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if (p(a)) SCons(() => a, () => b) else b)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case SCons(h, t) if p(h()) => SCons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def append[B >: A](x: => B): Stream[B] =
    foldRight(Stream[B](x))((a, b) => SCons(() => a, () => b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(Stream[B]())((a, b) => {
      f(a).foldRight(b)((x, y) => y append x)
    }).foldRight(Stream[B]())((a, b) => b append a)

  def headOption: scala.Option[A] =
    foldRight[scala.Option[A]](scala.None)((_, b) => this match {
      case SCons(h, _) => scala.Some(h())
      case _ => b
    })

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case SCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def drop(n: Int): List[A] = (n, this) match {
    case (_, Empty) => List()
    case (0, SCons(h, t)) => List(h()) ++ t().toList
    case (_, SCons(_, t)) => t().drop(n - 1)
  }
}

case object Empty extends Stream[Nothing]
case class SCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    SCons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def unfold[A, S](z: S)(f: S => scala.Option[(A, S)]): Stream[A] = f(z) match {
    case scala.None => Empty
    case scala.Some((a, s)) => cons(a, unfold(s)(f))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs2: Stream[Int] = cons(0, cons(1, unfold(0, 1)(s => {
    scala.Some(s._1 + s._2, (s._2, s._1 + s._2))
  })))

  def from2(n: Int): Stream[Int] = unfold(n)(s => scala.Some(s, s + 1))

  def constant2(n: Int): Stream[Int] = unfold(n)(s => scala.Some(s, s))
  def ones2: Stream[Int] = constant2(1)

  def fibs: Stream[Int] = {
    def fibsHelper(p1: Int, p2: Int): Stream[Int] =
      cons(p1 + p2, fibsHelper(p2, p1 + p2))
    cons(0, cons(1, fibsHelper(0, 1)))
  }

  def apply[A](as: A*): Stream[A] =
    if (as isEmpty) empty else cons(as.head, apply(as.tail: _*))
}