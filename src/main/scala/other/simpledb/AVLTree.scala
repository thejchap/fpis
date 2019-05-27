package simpledb

import scala.math.max

sealed trait AVLTree[+T] {
  def key: T = ???
  def lr: Tuple2[AVLTree[T], AVLTree[T]] = (Nil, Nil)
  def height: Int = 0
  def balance: Int = 0
}

case object Nil extends AVLTree[Nothing]
case class Node[T](
  override val key: T,
  override val lr: Tuple2[AVLTree[T], AVLTree[T]],
  override val height: Int,
  override val balance: Int) extends AVLTree[T]

object AVLTree {
  def insert[T](t: AVLTree[T], k: T, compare: (T, T) => Int): AVLTree[T] =
    balance(bstInsert(t, k, compare))

  private def balance[T](t: AVLTree[T]) = t balance match {
    case 0 | 1 | -1 => t
    case 2 => rotateLeft(t)
    case -2 => rotateRight(t)
    case _ => Nil
  }

  private def rotateLeft[T](t: AVLTree[T]) = {
    val (_, z) = t.lr
    val (t23, t4) = z.lr
    val newT = apply(t.key, (Nil, t23), t23.height + 1, t23.height)
    apply(z.key, (newT, t4), 1 + max(newT.height, t4.height), t4.height - newT.height)
  }

  private def rotateRight[T](t: AVLTree[T]) = {
    val (z, _) = t.lr
    val (t4, t23) = z.lr
    val newT = apply(t.key, (t23, Nil), t23.height + 1, t23.height)
    apply(z.key, (t4, newT), 1 + max(newT.height, t4.height), newT.height - t4.height)
  }

  private def bstInsert[T](t: AVLTree[T], k: T, compare: (T, T) => Int) = t match {
    case Nil => apply(k)
    case Node(x, lr, h, b) if compare(k, x) == 0 => apply(k, lr, h, b)
    case Node(x, (l, r), h, b) if compare(k, x) < 0 => {
      val l2 = insert(l, k, compare)
      apply(x, (l2, r), 1 + max(l2.height, r.height),  r.height - l2.height)
    }
    case Node(x, (l, r), h, b) if compare(k, x) > 0 => {
      val r2 = insert(r, k, compare)
      apply(x, (l, r2), 1 + max(l.height, r2.height),  r2.height - l.height)
    }
  }

  def apply[T](
    k: T,
    lr: Tuple2[AVLTree[T], AVLTree[T]] = (Nil, Nil),
    h: Int = 1,
    b: Int = 0) = Node(k, lr, h, b)
}