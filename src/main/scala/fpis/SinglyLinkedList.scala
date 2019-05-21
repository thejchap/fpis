package fpis

sealed trait SinglyLinkedList[+A]
case object Nil extends SinglyLinkedList[Nothing]
case class Cons[+A](head: A, tail: SinglyLinkedList[A]) extends SinglyLinkedList[A]

object SinglyLinkedList {
  def sum(ints: SinglyLinkedList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: SinglyLinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: SinglyLinkedList[A], x: A): SinglyLinkedList[A] = l match {
    case Nil => apply(x)
    case Cons(_, xs) => Cons(x, xs)
  }

  def drop[A](l: SinglyLinkedList[A], n: Int): SinglyLinkedList[A] = (l, n) match {
    case (Nil, _) => Nil
    case (_, 0) => l
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: SinglyLinkedList[A], f: A => Boolean): SinglyLinkedList[A] = apply()

  def apply[A](as: A*): SinglyLinkedList[A] =
    if (as isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}