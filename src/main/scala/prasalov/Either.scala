package prasalov

/**
 * Created by kirillprasalov on 08.04.16.
 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this map f match {
    case Left(e) => Left(e)
    case Right(a) => a
  }

  def orElse[EE >: E, B >: A](b : => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap((a1) => b map((b1) => f(a1, b1)))
  /*
    this match {
    case Left(e) => Left(e)
    case Right(a) => b map((b1) => f(a, b1))
  }*/
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]