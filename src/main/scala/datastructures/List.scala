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
    case Cons(_, t) =>
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

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
   * ex.3.6
   * TODO 末尾再帰版も考える
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil        => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /**
   * ex.3.7
   * 短絡できない。
   * 短絡するとしたらfの中で行うしかないが、再帰はfの処理が始まる前に実行されるため。
   */

  /**
   * ex.3.8
   * コピーするだけ
   */
  def ex3_8[A](l: List[A]): List[A] = {
    foldRight(l, Nil: List[A])(Cons(_, _))
  }

  /**
   * ex.3.9
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, l) => l + 1)
  }

  /**
   * ex.3.10
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * ex.3.11
   */
  def sumViaFoldLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }
  def productViaFoldLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  /**
   * ex.3.12
   */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])(flip(Cons(_, _)))
  }

  /**
   * ex.3.13
   */
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)(flip(f))
  }
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)(flip(f))
  }

  /**
   * ex.3.14
   */
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  /**
   * ex.3.15
   */
  def flatten[A](ll: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(ll, Nil: List[A])(appendViaFoldRight)
  }

  /**
   * ex.3.16
   */
  def addOne(ints: List[Int]): List[Int] = {
    foldRightViaFoldLeft(ints, Nil: List[Int])((i, l) => Cons(i + 1, l))
  }

  /**
   * ex.3.17
   */
  def doublesToString(ds: List[Double]): List[String] = {
    foldRightViaFoldLeft(ds, Nil: List[String])((d, l) => Cons(d.toString, l))
  }

  /**
   * ex.3.18
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(as, Nil: List[B])((a, bs) => Cons(f(a), bs))
  }

  /**
   * ex.3.19
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(as, Nil: List[A])((a, as) => {
      if (f(a)) Cons(a, as)
      else as
    })
  }

  /**
   * ex.3.20
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  /**
   * ex.3.21
   */
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) { a =>
      if (f(a))
        List(a)
      else
        Nil
    }
  }

  /**
   * ex.3.22
   */
  def ex3_22[A](l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(i1, t1), Cons(i2, t2)) => Cons(i1 + i2, ex3_22(t1, t2))
    case _                            => Nil
  }

  /**
   * ex.3.23
   */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    case _                          => Nil
  }

  /**
   * ex.3.24
   * Nilはどんなリストに対してもsubsequenceとする
   */
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (startsWith(sup, sub)) {
      true
    } else {
      sup match {
        case Nil         => sub == Nil
        case Cons(_, xs) => hasSubsequence(xs, sub)
      }
    }
  }
  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)                             => true
    case (Cons(suph, supt), Cons(subh, subt)) =>
      if (suph == subh) {
        startsWith(supt, subt)
      } else {
        false
      }
    case (_, _)                               => false
  }
}
