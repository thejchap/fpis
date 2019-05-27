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

  def apply[T](k: T, lr: Tuple2[AVLTree[T], AVLTree[T]] = (Nil, Nil)) = {
    Node(k, lr, makeHeight(lr._1, lr._2), makeBalanceFactor(lr._1, lr._2))
  }

  private def balance[T](t: AVLTree[T]) = t balance match {
    case 0 | 1 | -1 => t
    case 2 => rotateLeft(t)
    case -2 => rotateRight(t)
    case _ => Nil
  }

  private def rotateLeft[T](t: AVLTree[T]) = {
    val (_, r) = t.lr
    val (l2, r2) = r.lr
    apply(r.key, (apply(t.key, (Nil, l2)), r2))
  }

  private def rotateRight[T](t: AVLTree[T]) = {
    val (l, _) = t.lr
    val (l2, r2) = l.lr
    apply(l.key, (l2, apply(t.key, (r2, Nil))))
  }

  private def bstInsert[T](t: AVLTree[T], k: T, compare: (T, T) => Int) = t match {
    case Nil => apply(k)
    case Node(x, lr, _, _) if compare(k, x) == 0 => apply(k, lr)
    case Node(x, (l, r), _, _) if compare(k, x) < 0 => apply(x, (insert(l, k, compare), r))
    case Node(x, (l, r), _, _) if compare(k, x) > 0 => apply(x, (l, insert(r, k, compare)))
  }

  private def makeBalanceFactor[T](l: AVLTree[T], r: AVLTree[T]) = r.height - l.height
  private def makeHeight[T](l: AVLTree[T], r: AVLTree[T]) = 1 + max(l.height, r.height)
}