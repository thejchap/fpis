package fpis

import math.pow

object Ch4 {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(l: Seq[Double]): Option[Double] =
      if (l isEmpty) None
      else Some(l.sum / l.length)

    mean(xs).flatMap(m => mean(xs.map(x => pow(x - m, 2))))
  }
}