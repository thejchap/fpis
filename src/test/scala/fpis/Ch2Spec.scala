import org.scalatest.FunSpec
import fpis.Ch2

class Ch2Spec extends FunSpec {
  describe("fib") {
    it("returns the nth fibonnaci number") {
      assert(Ch2.fib(6) == 8)
    }
  }
}