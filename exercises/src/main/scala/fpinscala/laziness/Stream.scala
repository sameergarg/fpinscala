package fpinscala.laziness

import Stream._

import scala.collection.immutable

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = {
    val temp = new scala.collection.mutable.ListBuffer[A]

    def loop(rem: Stream[A]): List[A] = rem match {
      case Empty => temp.toList
      case Cons(h, t) =>
        temp += h()
        loop(t())
    }

    loop(this)
  }

  //foldRight(List())((next, acc) => next() :: acc)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if (n <= 0) => Empty
    case Cons(h, _) if (n == 1) => cons(h(), Empty)
    case Cons(h, t) => cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)((a, as) => cons(a, as))

  def flatmap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (_, size) if(size <= 0) => None
    case (Empty, _) => None
    case (Cons(h, t), size) => Some((h(), (t(), size - 1)))
  }

  def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) => if(p(h())) Some((h(), t())) else None
    case Empty => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
    case _ => None
  }

  def hasSubsequence[A](s2: Stream[A]): Boolean = tails exists(_ startsWith s2)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[A](s: Stream[A]): Boolean = this zipAll s find( pair => pair._1 != pair._2 && pair._2 != None) isEmpty

  def startsWith2[A](s: Stream[A]): Boolean = zipAll(s) takeWhile(_._2 isEmpty) forAll { case(s1, s2) => s1 == s2 }

  def tails: Stream[Stream[A]] = unfold(this){
    case s @ Cons(_, t) => Some((s, t()))
    case _ => None
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))){ (a, pair) =>
    lazy val (cumulativeResult, acc) = pair
    lazy val next: B = f(a, cumulativeResult)
    (next, cons(next, acc))
  }._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(n0: Int, n1: Int): Stream[Int] = cons(n0, (loop(n1, n0 + n1)))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = unfoldUsingPatMat(z)(f)

  private def unfoldUsingPatMat[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def unfoldUsingFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).fold(empty[A])(pair => cons(pair._1, unfoldUsingFold(pair._2)(f)))

  def unfoldUsingMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map(pair => cons(pair._1, unfoldUsingFold(pair._2)(f))).getOrElse(empty[A])

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  val onesUsingUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  val fibsUsingUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }
}