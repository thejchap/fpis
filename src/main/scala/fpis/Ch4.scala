package fpis

import math.pow

object Ch4 {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(l: Seq[Double]): Option[Double] =
      if (l isEmpty) None
      else Some(l.sum / l.length)

    mean(xs).flatMap(m => mean(xs.map(x => pow(x - m, 2))))
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    try Some(a.map { case Some(a) => a })
    catch { case e: Exception => None }
}