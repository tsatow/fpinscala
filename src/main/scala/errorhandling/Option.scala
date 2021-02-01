package errorhandling

import scala.::

sealed trait Option[+A] {
  /**
   * Exercise.4.1
   */
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  /**
   * Exercise.4.1
   */
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None      => None
  }

  /**
   * Exercise.4.1
   */
  def getOrElse[B >: A](default : => B): B = this match {
    case Some(get) => get
    case None      => default
  }

  /**
   * Exercise.4.1
   */
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None    => ob
  }

  /**
   * Exercise.4.1
   */
  def filter(f: A => Boolean): Option[A] = this flatMap { get =>
    if (f(get)) {
      this
    } else {
      None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  /**
   * Exercise.4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m => mean(xs.map {x => math.pow(x - m, 2)})}
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Exercise.4.3
   */
  def map2[A,B,C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] = aOpt flatMap { a => bOpt map { b => f(a, b) } }

  /**
   * Exercise.4.4
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])(map2(_, _)(_ :: _))
  }

  /**
   * Exercise.4.5
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]]) { (aa, acc) =>
      map2(f(aa), acc)(_ :: _)
    }
  }
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)
  }

}