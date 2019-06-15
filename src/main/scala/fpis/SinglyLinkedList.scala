package fpis

import annotation.tailrec

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

  def foldRight[A, B](as: SinglyLinkedList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: SinglyLinkedList[A], z: B)(f: (B, A) => B): B = {
    @tailrec def g(b: B, l: SinglyLinkedList[A]): B = l match {
      case Nil => b
      case Cons(x, xs) => g(f(b, x), xs)
    }
    g(z, as)
  }

  def reverse[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] =
    foldLeft(l, apply[A]())((y, x) => Cons(x, y))

  def foldLeft2[A, B](as: SinglyLinkedList[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))

  def sum2(ns: SinglyLinkedList[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: SinglyLinkedList[Double]) = foldRight(ns, 1.0)(_ * _)
  def length[A](as: SinglyLinkedList[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  def sum3(ns: SinglyLinkedList[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: SinglyLinkedList[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3[A](as: SinglyLinkedList[A]): Int = foldLeft(as, 0)((y, _) => y + 1)

  def sum4(ns: SinglyLinkedList[Int]) = foldLeft2(ns, 0)(_ + _)

  def tail[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: SinglyLinkedList[A], x: A): SinglyLinkedList[A] = l match {
    case Nil => apply(x)
    case Cons(_, xs) => Cons(x, xs)
  }

  def append[A](l: SinglyLinkedList[A], x: A): SinglyLinkedList[A] = l match {
    case Nil => apply(x)
    case Cons(y, Nil) => apply(y, x)
    case Cons(y, t) => Cons(y, append(t, x))
  }

  def drop[A](l: SinglyLinkedList[A], n: Int): SinglyLinkedList[A] = (l, n) match {
    case (Nil, _) => l
    case (_, 0) => l
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def concat[A](ls: SinglyLinkedList[A]*): SinglyLinkedList[A] = {
    @tailrec def concatHelper(l: SinglyLinkedList[A], t: SinglyLinkedList[A]*): SinglyLinkedList[A] =
      if (t isEmpty) l
      else concatHelper(foldLeft(t.head, l)((a, b) => append(a, b)), t.tail: _*)

    concatHelper(ls.head, ls.tail: _*)
  }

  def incrBy1(l: SinglyLinkedList[Int]): SinglyLinkedList[Int] = map(l)(_ + 1)
  def doubleToString(l: SinglyLinkedList[Double]): SinglyLinkedList[String] = map(l)(_ toString())

  def map[A, B](as: SinglyLinkedList[A])(f: A => B): SinglyLinkedList[B] = as match {
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
    case _ => Nil
  }

  def flatMap[A, B](as: SinglyLinkedList[A])(f: A => SinglyLinkedList[B]): SinglyLinkedList[B] = as match {
    case Cons(x, xs) => concat(f(x), flatMap(xs)(f))
    case _ => Nil
  }

  def filter[A](as: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] = as match {
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
    case _ => Nil
  }

  def flatMapFilter[A](as: SinglyLinkedList[A])(f: A => Boolean): SinglyLinkedList[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def sumElements(l1: SinglyLinkedList[Int], l2: SinglyLinkedList[Int]): SinglyLinkedList[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, t1), Cons(y, t2)) => Cons(x + y, sumElements(t1, t2))
  }

  def zipWith[A](l1: SinglyLinkedList[A], l2: SinglyLinkedList[A])(f: (A, A) => A): SinglyLinkedList[A] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Cons(x, t1), Cons(y, t2)) => Cons(f(x, y), zipWith(t1, t2)(f))
  }

  def apply[A](as: A*): SinglyLinkedList[A] =
    if (as isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def init[A](l: SinglyLinkedList[A]): SinglyLinkedList[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }
}