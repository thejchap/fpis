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
  describe("Exercise 5.2 - Stream#drop") {
    it("skips the first n elements") {
      val stream = Stream(1, 2, 3)

      assert(stream.drop(2) == List(3))
    }
  }
  describe("Exercise 5.3 - Stream#takeWhile") {
    it("returns elements while predicate is true") {
      val stream = Stream(1, 2, 3)

      assert(stream.takeWhile(_ < 3).toList == List(1, 2))

      val stream2 = Stream(3, 2, 1)

      assert(stream2.takeWhile(_ > 2).toList == List(3))
    }
  }
  describe("Exercise 5.4 - Stream#forAll") {
    it("checks if all elements match a given predicate") {
      val stream = Stream(1, 2, 3)

      assert(stream.forAll(_ < 4) == true)

      val stream2 = Stream(1, 2, 3)

      assert(stream2.forAll(_ < 2) == false)
    }
  }
  describe("Exercise 5.5 - Stream#takeWhile2") {
    it("returns elements while predicate is true") {
      val stream = Stream(1, 2, 3)

      assert(stream.takeWhile2(_ < 3).toList == List(1, 2))

      val stream2 = Stream(3, 2, 1)

      assert(stream2.takeWhile2(_ > 2).toList == List(3))
    }
  }
  describe("Exercise 5.6 - Stream#headOption") {
    it("optionally extracts head from stream") {
      val stream = Stream(1, 2, 3)

      assert(stream.headOption == Some(1))
    }
  }
}