import org.scalatest.FunSpec

import fpis.{SimpleRNG, RNG}

class Ch6Spec extends FunSpec {
  describe("Exercise 6.1 - nonNegativeInt") {
    it("returns a random non negative integer") {
      def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        val n = if (i < 0) -(i + 1) else i
        (n, r)
      }

      val (n, _) = nonNegativeInt(SimpleRNG(1))

      assert(n > 0 === true)
    }
  }
  describe("Exercise 6.2 - double") {
    it("generates a double between 0 and 1") {
      def double(rng: RNG): (Double, RNG) = {
        val (i, r) = rng.nextInt
        val n = (if (i < 0) -i else i).toDouble
        val max = Int.MaxValue.toDouble
        (n / max, r)
      }

      val (n, _) = double(SimpleRNG(1))
      assert(n > 0 === true)
      assert(n < 1 === true)
    }
  }
}