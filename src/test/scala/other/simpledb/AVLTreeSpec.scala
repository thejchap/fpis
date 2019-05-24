import org.scalatest.{FunSpec, BeforeAndAfter}
import simpledb.{AVLTree, Nil}

class AVLTreeSpec extends FunSpec with BeforeAndAfter {
  var compare: (String, String) => Int = _

  before {
    compare = (a: String, b: String) => {
      if (a > b) 1
      else if (a < b) -1
      else 0
    }
  }

  describe("AVLTree") {
    describe("insert") {
      it("inserts a leaf if the tree is Nil") {
        val result = AVLTree.insert(Nil, "a", compare)
        assert(result.k == "a")
        assert(result.lr == (Nil, Nil))
        assert(result.h == 1)
      }
      it("overwrites k if new k == k") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "a", compare)

        assert(tree2.h == 1)
        assert(tree2.k == "a")

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(l == Nil)
        assert(r == Nil)
      }
      it("inserts a leaf to the right if the key is > current key") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "c", compare)

        assert(tree2.h == 2)
        assert(tree2.k == "a")

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(l == Nil)
        assert(r.k == "c")
        assert(r.h == 1)
      }
      it("inserts a leaf to the left if the key is < current key") {
        val tree1 = AVLTree.insert(Nil, "c", compare)
        val tree2 = AVLTree.insert(tree1, "a", compare)

        assert(tree2.h == 2)
        assert(tree2.k == "c")

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(r == Nil)
        assert(l.k == "a")
        assert(l.h == 1)
      }
      it("recursively inserts right") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "c", compare)
        val tree = AVLTree.insert(tree2, "e", compare)

        assert(tree.k == "a")
        assert(tree.h == 3)
      }
    }
  }
}