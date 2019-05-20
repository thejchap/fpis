package fpis

import annotation.tailrec

object Ch2 {
    // Exercise 2.1 - Naive
    def fib1(n: Int): Int = n match {
        case 0 | 1 => n
        case _ => fib1(n - 1) + fib1(n - 2)
    }

    // Exercise 2.1 - Local tail-recursive
    def fib2(n: Int): Int = {
        @tailrec def fib(m: Int, x: Int, y: Int): Int = m + 2 match {
            case `n` => x + y
            case _ => fib(m + 1, y, x + y)
        }
        fib(0, 0, 1)
    }

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
      as.sliding(2).find(a => !ordered(a(0), a(1))) isEmpty

    // Exercise 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
}