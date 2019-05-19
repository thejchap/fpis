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
    it("returns whether an array is sorted via a given function") (pending)
  }
}