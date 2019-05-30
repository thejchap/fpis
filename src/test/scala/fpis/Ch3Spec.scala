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
    describe("Exercise 3.3 - setHead") {
      it("replaces the head of the list") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(6, 2, 3, 4, 5)

        assert(SinglyLinkedList.setHead(list1, 6) == list2)
      }
      it("works for Nil") {
        assert(SinglyLinkedList.setHead(Nil, 5) == SinglyLinkedList(5))
      }
    }
    describe("Exercise 3.4 - drop") {
      it("drops the first n elements from the list") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(4, 5)

        assert(SinglyLinkedList.drop(list1, 3) == list2)
      }
      it("works with 1 element") {
        val list = SinglyLinkedList(1)

        assert(SinglyLinkedList.drop(list, 3) == Nil)
      }
      it("works with 0 arg") {
        val list = SinglyLinkedList(1, 2)

        assert(SinglyLinkedList.drop(list, 0) == list)
      }
    }
    describe("Exercise 3.5 - dropWhile") {
      it("drops while elements match a given predicate") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(4, 5)
        val pred = (a: Int) => a < 4

        assert(SinglyLinkedList.dropWhile(list1, pred) == list2)
      }
    }
    describe("Exercise 3.6 - init") {
      it("creates a new list with all but the last element") {
        var list1 = SinglyLinkedList(1, 2, 3, 4)
        var list2 = SinglyLinkedList(1, 2, 3)

        assert(SinglyLinkedList.init(list1) == list2)
      }
    }
  }
}