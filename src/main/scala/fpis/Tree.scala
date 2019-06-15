package fpis

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A](tree: Tree[A])(f: A => A): Tree[A] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def size2[A](tree: Tree[A]): Int = fold(tree, 0)((_, b) => b + 1)(1 + _ + _)
  def maximum2(tree: Tree[Int]): Int = fold(tree, 0)((a, _) => a)(_ max _)
  def depth2[A](tree: Tree[A]): Int = fold(tree, 0)((_, _) => 1)(1 +_ max _)
  def map2[A](tree: Tree[A])(f: A => A): Tree[A] =
    fold(tree, tree)((a, _) => Leaf(f(a)))((l, r) => Branch(l, r))

  def fold[A, B](tree: Tree[A], a: B)(f: (A, B) => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value, a)
    case Branch(l, r) => g(fold(l, a)(f)(g), fold(r, a)(f)(g))
  }
}