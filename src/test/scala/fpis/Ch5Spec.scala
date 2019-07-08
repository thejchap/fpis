import org.scalatest.FunSpec

import fpis.Stream

class Ch5Spec extends FunSpec {
  describe("Exercise 5.1 - Stream#toList") {
    it("converts a Stream to a list") {
      val stream = Stream(1, 2, 3)

      assert(stream.toList == List(1, 2, 3))
    }
  }
  describe("Exercise 5.2 - Stream#take") {
    it("returns first n elements") {
      val stream = Stream(1, 2, 3)

      assert(stream.take(2) == List(1, 2))

      val stream2 = Stream(1, 2)

      assert(stream2.take(3) == List(1, 2))
    }
  }
}