import org.scalatest.FunSpec
import fpis.Ch2

class Ch2Spec extends FunSpec {
  describe("fib1") {
    it("returns the nth fibonnaci number") {
      assert(Ch2.fib1(6) == 8)
    }
  }

  describe("fib2") {
    it("returns the nth fibonnaci number with tail call optimization") {
      assert(Ch2.fib2(6) == 8)
    }
  }

  describe("isSorted") {
    it("returns true if an array is sorted") {
      assert(Ch2.isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
    }

    it("returns false if an array isn't sorted") {
      assert(!Ch2.isSorted(Array(3, 2, 1, 4), (x: Int, y: Int) => x < y))
    }
  }

  describe("curry") {
    it("curries") {
      val f1 = (a: Int, b: Int) => a + b
      val f2 = Ch2.curry(f1)

      assert(f2(1)(2) == 3)
    }
  }

  describe("uncurry") {
    it("uncurries") {
      val f1 = (a: Int, b: Int) => a + b
      val f2 = Ch2.curry(f1)
      val f3 = Ch2.uncurry(f2)

      assert(f3(1, 2) == 3)
    }
  }
}