package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =  t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => max(left).max(max(right))
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) =>  0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(lf: A => B)(comb: (B,B) => B): B = t match {
    case Leaf(v) => lf(v)
    case Branch(l, r) => comb(fold(l)(lf)(comb), fold(r)(lf)(comb))
  }

  def sizeUsingFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maxUsingFold(t: Tree[Int]): Int = fold(t)(identity)(_.max(_))

  def depthUsingFold(t: Tree[Int]): Int = fold(t)(_ => 0)((d1, d2) => 1 + d1 max d2)

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))


}