package laziness

import scala.::

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  /**
   * Exercise.5.1
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(st: Stream[A], acc: List[A]): List[A] = st match {
      case Empty      => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }

  /**
   * Exercise.5.2
   * TODO tailrec
   */
  def take(n: Int): Stream[A] = this match {
    case Empty      => Empty
    case Cons(h, t) =>
      if (n <= 0) {
        Empty
      } else {
        Stream.cons(h(), t().take(n - 1))
      }
  }

  /**
   * Exercise.5.2
   * TODO tailrec
   */
  def drop(n: Int): Stream[A] = this match {
    case Empty      => Empty
    case Cons(_, t) =>
      if (n <= 0) {
        this
      } else {
        t().drop(n - 1)
      }
  }

  /**
   * Exercise.5.3
   * TODO tailrec
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty      => Empty
    case Cons(h, t) =>
      if (p(h())) {
        Stream.cons(h(), t().takeWhile(p))
      } else {
        Empty
      }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case Empty      => z
  }

  /**
   * Exercise.5.4
   */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  /**
   * Exercise.5.5
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) {
        Stream.cons(a, b)
      } else {
        Stream.empty
      }
    }
  }

  /**
   * Exercise.5.6
   */
  def headOptionViaFoldRight: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  /**
   * Exercise.5.7
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty)((a, b) => Stream.cons(f(a), b))
  }

  /**
   * Exercise.5.7
   */
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty) { (a, b) =>
      if (f(a)) {
        Stream.cons(a, b)
      } else {
        b
      }
    }
  }

  /**
   * Exercise.5.7
   */
  def append(st: Stream[A]): Stream[A] = {
    foldRight(st)((a, b) => Stream.cons(a, b))
  }

  /**
   * Exercise.5.7
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((a, b) => f(a).append(b))
  }
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

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) {
      Empty
    } else {
      cons(as.head, apply(as.tail: _*))
    }
  }
}
