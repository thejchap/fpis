package simpledb

import scala.math.max

sealed trait AVLTree[+T] {
  def k: T = ???
  def lr: Tuple2[AVLTree[T], AVLTree[T]] = (Nil, Nil)
  def h: Int = 0
}

case object Nil extends AVLTree[Nothing]
case class Node[T](
  override val k: T,
  override val lr: Tuple2[AVLTree[T], AVLTree[T]],
  override val h: Int) extends AVLTree[T]

object AVLTree {
  def insert[T](t: AVLTree[T], k: T, compare: (T, T) => Int): AVLTree[T] = t match {
    case Nil => apply(k)
    case Node(x, lr, h) if compare(k, x) == 0 => apply(k, lr, h)
    case Node(x, (l, r), h) if compare(k, x) < 0 => {
      val l2 = insert(l, k, compare)
      apply(x, (l2, r), 1 + max(r.h, l2.h))
    }
    case Node(x, (l, r), h) if compare(k, x) > 0 => {
      val r2 = insert(r, k, compare)
      apply(x, (l, r2), 1 + max(l.h, r2.h))
    }
  }

  def apply[T](
    k: T,
    lr: Tuple2[AVLTree[T], AVLTree[T]] = (Nil, Nil),
    h: Int = 1): AVLTree[T] = Node(k, lr, h)
}