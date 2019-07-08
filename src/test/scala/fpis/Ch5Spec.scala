import org.scalatest.FunSpec

import fpis.Stream

class Ch5Spec extends FunSpec {
  describe("Exercise 5.1 - Stream#toList") {
    it("converts a Stream to a list") {
      val stream = Stream(1, 2, 3)

      assert(stream.toList == List(1, 2, 3))
    }
  }
}