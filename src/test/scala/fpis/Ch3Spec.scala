import org.scalatest.FunSpec
import fpis.SinglyLinkedList

class Ch3Spec extends FunSpec {
  describe("SinglyLinkedList") {
    describe("sum") {
      it("sums elements in the list") {
        assert(SinglyLinkedList.sum(SinglyLinkedList(1, 2, 3)) == 6)
      }
    }
    describe("product") {
      it("multiplies elements in the list") {
        assert(SinglyLinkedList.product(SinglyLinkedList(3.0, 5.0)) == 15.0)
      }
    }
  }
}