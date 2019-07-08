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