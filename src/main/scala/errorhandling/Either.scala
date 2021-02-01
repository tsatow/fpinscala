package errorhandling

sealed trait Either[+E, +A] {
  /**
   * Exercise.4.6
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v)  => Left(v)
    case Right(v) => Right(f(v))
  }

  /**
   * Exercise.4.6
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v)  => Left(v)
    case Right(v) => f(v)
  }

  /**
   * Exercise.4.6
   */
  def orElse[EE >: E, AA >: A](ob: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_)  => ob
    case Right(v) => Right(v)
  }

  /**
   * Exercise.4.6
   */
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = {
    this flatMap { aa => b map { bb => f(aa, bb) } }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value:A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e:Exception=>Left(e) }

  /**
   * Exercise.4.7
   */
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))
  }

  /**
   * Exercise.4.7
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(identity)
  }

  /**
   * Exercise.4.8
   * Validationのようなデータ型を使う
   * @see http://slides.pab-tech.net/either-and-validation/
   */
}


