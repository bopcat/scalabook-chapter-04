package prasalov

/**
 * Created by kirillprasalov on 07.04.16.
 */
object App {

  def main(args: Array[String]): Unit = {
    println("Ex. 4.1")

    println("map of Some: " + Some(4).map(_ * 2))
    println("map of None: " + None.asInstanceOf[Option[Int]].map(_ * 2))
    println("map of Some with exception: " + Some(4).map(_ / 0))
    println()

    println("flatmap of Some: " + Some(4).flatMap(intToOption))
    println("flatmap of None: " + None.asInstanceOf[Option[Int]].flatMap(intToOption))
    println("flatmap of Some with exception: " + Some(0).flatMap(intToOption))
    println()

    println("getOrElse of Some: " + Some(4).getOrElse(0))
    println("getOrElse of None: " + None.asInstanceOf[Option[Int]].getOrElse(0))
    println()

    println("orElse of Some: " + Some(4).orElse(Some(0)))
    println("orElse of None: " + None.asInstanceOf[Option[Int]].orElse(Some(0)))
    println()

    println("filter of Some holding a predicate: " + Some(4).filter(isNotZero))
    println("filter of Some failing a predicate: " + Some(0).filter(isNotZero))
    println("filter of None: " + None.asInstanceOf[Option[Int]].filter(isNotZero))
    println()

    println("**********")
    println()

    println("Ex. 4.3")

    println("variance: " + variance(List(0.1, 0.2, 0.3)))
    println("variance of an empty list: " + variance(Nil))

    println()
    println("**********")
    println()

    println("Ex. 4.4")

    println("sequence: " + Option.sequence(List(Some(1), Some(2), Some(3))))
    println("sequence with None: " + Option.sequence(List(Some(1), None, Some(3))))

    println()
    println("**********")
    println()

    println("Ex. 4.5")

    println("traverse: " + Option.traverse(List(1, 2, 3))(divideByZero))
    println("traverse with None: " + Option.traverse(List(1, 0, 3))(divideByZero))
  }

  def intToOption(x: Int) = if (x == 0) None else Some(x)
  def isNotZero(x: Int) = x != 0
  def divideByZero(x: Int) = if (x == 0) None else Some(10 / x)

  def mean(ys: Seq[Double]) : Option[Double] = ys match {
    case Nil => None
    case _ => Some(ys.foldLeft(0.0)(_ + _) / ys.size)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .map(
        (m) => xs.map(
          (x) => math.pow(x - m, 2)
        )
      )
      .flatMap(mean)
}
