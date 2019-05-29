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
  type Children[T] = (AVLTree[T], AVLTree[T])

  def insert[T](t: AVLTree[T], k: T, cmp: (T, T) => Int): AVLTree[T] =
    balance(bstInsert(t, k, cmp))

  def search[T](t: AVLTree[T], k: T, cmp: (T, T) => Int): Option[T] = t match {
    case Nil => None
    case Node(x, lr, _, _) if cmp(k, x) == 0 => Some(x)
    case Node(x, (l, r), _, _) if cmp(k, x) < 0 => search(t.lr._1, k, cmp)
    case Node(x, (l, r), _, _) if cmp(k, x) > 0 => search(t.lr._2, k, cmp)
  }

  def apply[T](k: T, lr: Children[T] = (Nil, Nil)) =
    Node(k, lr, makeHeight(lr), makeBalanceFactor(lr))

  private def bstInsert[T](t: AVLTree[T], k: T, cmp: (T, T) => Int) = t match {
    case Nil => apply(k)
    case Node(x, lr, _, _) if cmp(k, x) == 0 => apply(k, lr)
    case Node(x, (l, r), _, _) if cmp(k, x) < 0 => apply(x, (insert(l, k, cmp), r))
    case Node(x, (l, r), _, _) if cmp(k, x) > 0 => apply(x, (l, insert(r, k, cmp)))
  }

  private def balance[T](t: AVLTree[T]) = t balance match {
    case 0 | 1 | -1 => t
    case 2 =>
      rotate(t,
        (lr: Children[T]) => lr._2,
        (k: T, lr: Children[T]) => (apply(k, (Nil, lr._1)), lr._2))
    case -2 =>
      rotate(t,
        (lr: Children[T]) => lr._1,
        (k: T, lr: Children[T]) => (lr._1, apply(k, (lr._2, Nil))))
    case _ => Nil
  }

  private def makeBalanceFactor[T](lr: Children[T]) = lr._2.height - lr._1.height
  private def makeHeight[T](lr: Children[T]) = 1 + max(lr._1.height, lr._2.height)
  private def rotate[T](t: AVLTree[T], f: Children[T] => AVLTree[T], g: (T, Children[T]) => Children[T]) =
    apply(f(t lr).key, g(t key, f(t lr) lr))
}