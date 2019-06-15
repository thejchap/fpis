package fpis

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case _ => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case _ => None
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case _ => None
  }
}

case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]