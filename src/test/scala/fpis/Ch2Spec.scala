import org.scalatest.FunSpec
import fpis.Ch2

class Ch2Spec extends FunSpec {
  describe("Exercise 2.1 - fib1") {
    it("returns the nth fibonnaci number") {
      assert(Ch2.fib1(6) == 8)
    }
  }

  describe("Exercise 2.1 - fib2") {
    it("returns the nth fibonnaci number with tail call optimization") {
      assert(Ch2.fib2(6) == 8)
    }
  }

  describe("Exercise 2.2 - isSorted") {
    it("returns true if an array is sorted") {
      assert(Ch2.isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
    }

    it("returns false if an array isn't sorted") {
      assert(!Ch2.isSorted(Array(3, 2, 1, 4), (x: Int, y: Int) => x < y))
    }
  }

  describe("Exercise 2.3 - curry") {
    it("converts a 2 argument function into a 1 argument function that partially applies f") {
      val f1 = (a: Int, b: Int) => a + b
      val f2 = Ch2.curry(f1)

      assert(f2(1)(2) == 3)
    }
  }

  describe("Exercise 2.4 - uncurry") {
    it("reverses curry") {
      val f1 = (a: Int, b: Int) => a + b
      val f2 = Ch2.curry(f1)
      val f3 = Ch2.uncurry(f2)

      assert(f3(1, 2) == 3)
    }
  }

  describe("Exercise 2.5 - compose") {
    it("feeds the output of one function into the input of another") {
      val f1 = (b: Int) => b * 2
      val f2 = (a: Int) => a + 1
      val f3 = Ch2.compose(f1, f2)

      assert(f3(3) == 8)
    }
  }
}