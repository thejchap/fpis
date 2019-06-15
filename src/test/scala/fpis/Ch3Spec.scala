import org.scalatest.FunSpec
import fpis.{SinglyLinkedList, Cons, Nil, Tree, Leaf, Branch}

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

        assert(SinglyLinkedList.dropWhile(list1)(a => a < 4) == list2)
      }
    }
    describe("Exercise 3.6 - init") {
      it("creates a new list with all but the last element") {
        var list1 = SinglyLinkedList(1, 2, 3, 4)
        var list2 = SinglyLinkedList(1, 2, 3)

        assert(SinglyLinkedList.init(list1) == list2)
      }
    }
    describe("Listing 3.2 - foldRight") {
      it("is a refactor") {
        assert(SinglyLinkedList.sum2(SinglyLinkedList(1, 2, 3)) == 6)
        assert(SinglyLinkedList.product2(SinglyLinkedList(3.0, 5.0)) == 15.0)
      }
    }
    describe("Exercise 3.9 - length") {
      it("returns the length of the list") {
        assert(SinglyLinkedList.length(SinglyLinkedList(1, 2, 3)) == 3)
      }
    }
    describe("Exercise 3.10/11 - foldLeft") {
      it("folds left") {
        assert(SinglyLinkedList.sum3(SinglyLinkedList(1, 2, 3)) == 6)
        assert(SinglyLinkedList.product3(SinglyLinkedList(3.0, 5.0)) == 15.0)
        assert(SinglyLinkedList.length3(SinglyLinkedList(1, 2, 3)) == 3)
      }
    }
    describe("Exercise 3.12 - reverse") {
      it("reverses a list") {
        val list1 = SinglyLinkedList(1, 2, 3)
        val list2 = SinglyLinkedList(3, 2, 1)

        assert(SinglyLinkedList.reverse(list1) == list2)
      }
    }
    describe("Exercise 3.13 - foldLeft refactor") {
      it("does the same thing as foldLeft") {
        assert(SinglyLinkedList.sum4(SinglyLinkedList(1, 2, 3)) == 6)
      }
    }
    describe("Exercise 3.14 - append") {
      it("appends") {
        val list1 = SinglyLinkedList(1, 2, 3)
        val list2 = SinglyLinkedList.append(list1, 4)

        assert(list2 == SinglyLinkedList(1, 2, 3, 4))
      }
    }
    describe("Exercise 3.15 - concat") {
      it("concats") {
        val list1 = SinglyLinkedList(1, 2)
        val list2 = SinglyLinkedList(3, 4)
        val list3 = SinglyLinkedList(5, 6)
        val list4 = SinglyLinkedList.concat(list1, list2, list3)

        assert(list4 == SinglyLinkedList(1, 2, 3, 4, 5, 6))
      }
    }
    describe("Exercise 3.16/18 - incrBy1") {
      it("increments by 1") {
        val list1 = SinglyLinkedList(1, 2)
        val list2 = SinglyLinkedList(2, 3)
        val list3 = SinglyLinkedList.incrBy1(list1)

        assert(list3 == list2)
      }
    }
    describe("Exercise 3.17/18 - doubleToString") {
      it("increments by 1") {
        val list1 = SinglyLinkedList(1.0, 2.5)
        val list2 = SinglyLinkedList("1.0", "2.5")
        val list3 = SinglyLinkedList.doubleToString(list1)

        assert(list3 == list2)
      }
    }
    describe("Exercise 3.19 - filter") {
      it("filters") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(2, 4)
        val list3 = SinglyLinkedList.filter(list1)(_ % 2 == 0)

        assert(list3 == list2)
      }
    }
    describe("Exercise 3.20 - flatMap") {
      it("flat maps") {
        val list1 = SinglyLinkedList(1, 2, 3)
        val list2 = SinglyLinkedList(1, 1, 2, 2, 3, 3)
        val list3 = SinglyLinkedList.flatMap(list1)(i => SinglyLinkedList(i, i))

        assert(list3 == list2)
      }
    }
    describe("Exercise 3.21 - flatMapFilter") {
      it("filters") {
        val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
        val list2 = SinglyLinkedList(2, 4)
        val list3 = SinglyLinkedList.flatMapFilter(list1)(_ % 2 == 0)

        assert(list3 == list2)
      }
    }
    describe("Exercise 3.22 - sumElements") {
      it("sums elements in a list") {
        val list1 = SinglyLinkedList(1, 2, 3)
        val list2 = SinglyLinkedList(4, 5, 6)
        val list3 = SinglyLinkedList.sumElements(list1, list2)
        val list4 = SinglyLinkedList(5, 7, 9)

        assert(list3 == list4)
      }
    }
    describe("Exercise 3.23 - zipWith") {
      it("sums elements in a list") {
        val list1 = SinglyLinkedList(1, 2, 3)
        val list2 = SinglyLinkedList(4, 5, 6)
        val list3 = SinglyLinkedList.zipWith(list1, list2)(_ + _)
        val list4 = SinglyLinkedList(5, 7, 9)

        assert(list3 == list4)
      }
    }
    describe("Exercise 3.24 - hasSubSequence") {
      it("detects if subsequence is in list") {
        val list1 = SinglyLinkedList(1, 2, 3, 4)
        val list2 = SinglyLinkedList(1, 2)
        val res1 = SinglyLinkedList.hasSubSequence(list1, list2)

        assert(res1 == true)

        val list3 = SinglyLinkedList(1, 2, 3, 4)
        val list4 = SinglyLinkedList(1, 5)
        val res2 = SinglyLinkedList.hasSubSequence(list3, list4)

        assert(res2 == false)

        val list5 = SinglyLinkedList(1, 2, 3, 4)
        val list6 = SinglyLinkedList(3, 4)
        val res3 = SinglyLinkedList.hasSubSequence(list5, list6)

        assert(res3 == true)
      }
    }
    describe("Exercise 3.25 - Tree#size") {
      it("returns the number of nodes/branches in the tree") {
        val tree = Branch(Leaf(1), Leaf(2))

        assert(Tree.size(tree) == 3)

        val tree2 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(2))

        assert(Tree.size(tree2) == 5)
      }
    }
    describe("Exercise 3.26 - Tree#maximum") {
      it("returns the max value in the tree") {
        val tree = Branch(Leaf(1), Leaf(2))

        assert(Tree.maximum(tree) == 2)

        val tree2 = Branch(Branch(Leaf(1), Leaf(5)), Leaf(2))

        assert(Tree.maximum(tree2) == 5)
      }
    }
    describe("Exercise 3.27 - Tree#depth") {
      it("returns the depth") {
        val tree = Branch(Leaf(1), Leaf(2))

        assert(Tree.depth(tree) == 2)

        val tree2 = Branch(Branch(Leaf(1), Leaf(5)), Leaf(2))

        assert(Tree.depth(tree2) == 3)
      }
    }
    describe("Exercise 3.29 - Tree#map") {
      it("performs an operation on every node") {
        val tree = Branch(Leaf(1), Leaf(2))
        val tree2 = Branch(Leaf(2), Leaf(4))

        assert(Tree.map(tree)(_ * 2) == tree2)
      }
    }
    describe("Exercise 3.30 - Tree#fold") {
      it("performs an operation on every node") {
        val tree = Branch(Leaf(1), Leaf(2))
        val tree2 = Branch(Leaf(2), Leaf(4))

        assert(Tree.size2(tree) == 3)

        val tree3 = Branch(Leaf(1), Leaf(2))

        assert(Tree.maximum2(tree3) == 2)

        val tree4 = Branch(Branch(Leaf(1), Leaf(5)), Leaf(2))

        assert(Tree.maximum2(tree4) == 5)

        val tree5 = Branch(Leaf(1), Leaf(2))

        assert(Tree.depth2(tree5) == 2)

        val tree6 = Branch(Branch(Leaf(1), Leaf(5)), Leaf(2))

        assert(Tree.depth2(tree6) == 3)

        val tree7 = Branch(Leaf(1), Leaf(2))
        val tree8 = Branch(Leaf(2), Leaf(4))

        assert(Tree.map2(tree7)(_ * 2) == tree8)
      }
    }
  }
}