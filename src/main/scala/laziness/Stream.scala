package laziness

import laziness.Stream.unfold

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
   * Exercise.5.13
   */
  def takeViaUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), n) =>
        if (n > 0) {
          Some((h(), (t(), n - 1)))
        } else {
          None
        }
      case (Empty, _)      => None
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

  /**
   * Exercise.5.13
   */
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) => if (p(h())) Some((h(), t())) else None
      case Empty      => None
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
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  /**
   * Exercise.5.13
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty      => None
    }
  }

  /**
   * Exercise.5.7
   */
  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) { (a, b) =>
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
  def append[AA >: A](st: Stream[AA]): Stream[AA] = {
    foldRight(st)((a, b) => Stream.cons(a, b))
  }

  /**
   * Exercise.5.7
   */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty: Stream[B])((a, b) => f(a).append(b))
  }

  /**
   * Exercise.5.13
   */
  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _                          => None
    }
  }

  /**
   * Exercise.5.13
   */
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case (Cons(a, as), Cons(b, bs)) => Some(((Some(a()), Some(b())), (as(), bs())))
      case (Empty, Cons(b, bs))       => Some(((None, Some(b())), (Stream.empty[A], bs())))
      case (Cons(a, as), Empty)       => Some(((Some(a()), None), (as(), Stream.empty[B])))
      case (Empty, Empty)             => None
    }
  }

  /**
   * Exercise.5.14
   */
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _            => false
    } forAll {
      case (Some(a1), Some(a2)) => a1 == a2
      case _                    => false
    }
  }

  /**
   * Exercise.5.14
   */
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case s@Cons(_, t) => Some((s, t()))
      case Empty        => None
    }
  }

  /**
   * Exercise.5.16
   */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    val (_, result) = foldRight((z, Stream(z))) {
      case (a, (prev, st)) =>
        val curr = f(a, prev)
        (curr, Stream.cons(curr, st))
    }
    result
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

  /**
   * Exercise.5.8
   */
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  /**
   * Exercise.5.12
   */
  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some(a, a))
  }

  /**
   * Exercise.5.9
   */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  /**
   * Exercise.5.12
   */
  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n + 1))
  }

  /**
   * Exercise.5.10
   */
  val fibs: Stream[Int] = {
    def go(t1: Int, t2: Int): Stream[Int] = {
      cons(t1, go(t2, t1 + t2))
    }
    go(0, 1)
  }

  /**
   * Exercise.5.12
   */
  val fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) { case (t1, t2) => Some((t1, (t2, t1 + t2))) }
  }

  /**
   * Exercise.5.11
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None         => empty
  }

  /**
   * Exercise.5.12
   */
  val onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }

}
