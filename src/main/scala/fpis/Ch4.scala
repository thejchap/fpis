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

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    try Some(a.map { case Some(a) => a })
    catch { case e: Exception => None }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    try Some(a.map(x => f(x) match { case Some(y) => y }))
    catch { case e: Exception => None }
  }

  def sequence2[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    try Right(a.map { case Right(a) => a })
    catch { case e: E => Left(e) }

  def traverse2[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    try Right(a.map(x => f(x) match { case Right(y) => y }))
    catch { case e: E => Left(e) }
  }
}