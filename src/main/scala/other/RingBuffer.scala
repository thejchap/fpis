package ringbuffer

sealed trait RingBuffer[+A]
case object Nil extends RingBuffer[Nothing]
case class Cons[+A](head: A, tail: RingBuffer[A], last: A) extends RingBuffer[A]