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
}