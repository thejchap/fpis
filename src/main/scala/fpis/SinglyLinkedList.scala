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

  def tail[A](list: SinglyLinkedList[A]): SinglyLinkedList[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def apply[A](as: A*): SinglyLinkedList[A] =
    if (as isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}