import org.scalatest.FunSpec
import fpis.{SinglyLinkedList, Cons, Nil}

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
    describe("Exercise 3.1 - pattern match experiment") {
      it("matches") {
        val x = SinglyLinkedList(1, 2, 3, 4, 5) match {
          case Cons(x, Cons(2, Cons(4, _))) => x
          case Nil => 42
          case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
          case Cons(h, t) => h + SinglyLinkedList.sum(t)
          case _ => 101
        }

        assert(x == 3)
      }
    }
    describe("Exercise 3.2 - tail") {
      it("returns Nil if list is Nil") {
        assert(SinglyLinkedList.tail(Nil) == Nil)
      }
      it("removes the first element of the list if list is populated") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(2, 3, 4, 5)

        assert(SinglyLinkedList.tail(list1) == list2)
      }
    }
  }
}