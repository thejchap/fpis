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

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case SCons(h, t) if p(h()) => SCons(h, () => t().takeWhile(p))
    case _ => Empty
  }

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

  def apply[A](as: A*): Stream[A] =
    if (as isEmpty) empty else cons(as.head, apply(as.tail: _*))
}