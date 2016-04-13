package prasalov

/**
 * Created by kirillp on 12.04.16.
 */
sealed trait ResultOrErrors[+E, +A] {

  def map[B](f: A => B): ResultOrErrors[E, B] = this match {
    case Errors(e) => Errors(e)
    case Result(a) => Result(f(a))
  }

  def flatMap[EE >: E, B](f: A => ResultOrErrors[EE, B]): ResultOrErrors[EE, B] =
    this match {
      case Errors(e) => Errors(e)
      case _ => this map f match {
        case Errors(e) => Errors(e)
        case Result(a) => a
      }
    }

  // no changes to orElse, if there are any errors needs just to supply the new Result
  // there might be a case when the new result is also in fact an instance of Errors
  // for this the errors from this *could* be concatenated with those errors
  // however this really depends on the requirements
  def orElse[EE >: E, B >: A](b : => ResultOrErrors[EE, B]): ResultOrErrors[EE, B] = this match {
    case Errors(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: ResultOrErrors[EE, B])(f: (A, B) => C): ResultOrErrors[EE, C] =
    this flatMap((a1) => b map((b1) => f(a1, b1)))
}

case class Result[+A](get: A) extends ResultOrErrors[Nothing, A]
case class Errors[+E](errors: List[E]) extends ResultOrErrors[E, Nothing]