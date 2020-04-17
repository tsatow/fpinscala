package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /**
   * ex.3.2
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil        => Nil // 例外発生させる実装もあるかもしれない
    case Cons(_, t) => t
  }

  /**
   * ex.3.3
   */
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil        => Nil // 例外発生させる実装もあるかもしれない
    case Cons(_, t) => Cons(a, t)
  }

  /**
   * ex.3.4
   */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil        => Nil
    case Cons(h, t) =>
      if (n > 0) {
        drop(t, n -1)
      } else {
        l
      }
  }

  /**
   * ex.3.5
   */
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil        => Nil
    case Cons(h, t) =>
      if (f(h)) {
        dropWhile(t)(f)
      } else {
        l
      }
  }

}
