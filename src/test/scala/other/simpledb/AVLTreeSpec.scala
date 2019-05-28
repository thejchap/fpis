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
        assert(result.key == "a")
        assert(result.lr == (Nil, Nil))
        assert(result.height == 1)
        assert(result.balance == 0)
      }
      it("overwrites k if new k == k") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "a", compare)

        assert(tree2.height == 1)
        assert(tree2.key == "a")
        assert(tree2.balance == 0)

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(l == Nil)
        assert(r == Nil)
      }
      it("inserts a leaf to the right if the key is > current key") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "c", compare)

        assert(tree2.height == 2)
        assert(tree2.key == "a")
        assert(tree2.balance == 1)

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(l == Nil)
        assert(r.key == "c")
        assert(r.height == 1)
      }
      it("inserts a leaf to the left if the key is < current key") {
        val tree1 = AVLTree.insert(Nil, "c", compare)
        val tree2 = AVLTree.insert(tree1, "a", compare)

        assert(tree2.height == 2)
        assert(tree2.key == "c")
        assert(tree2.balance == -1)

        val l = tree2.lr._1
        val r = tree2.lr._2

        assert(r == Nil)
        assert(l.key == "a")
        assert(l.height == 1)
      }
      it("recursively inserts right") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "c", compare)
        val tree3 = AVLTree.insert(tree2, "e", compare)

        assert(tree3.key == "c")
        assert(tree3.height == 2)
        assert(tree3.balance == 0)
      }
      it("integration test 1") {
        val tree1 = AVLTree.insert(Nil, "b", compare)
        val tree2 = AVLTree.insert(tree1, "a", compare)
        val tree3 = AVLTree.insert(tree2, "c", compare)
        val tree4 = AVLTree.insert(tree3, "d", compare)
        val tree5 = AVLTree.insert(tree4, "e", compare)

        assert(tree5.key == "b")
        assert(tree5.height == 3)
        assert(tree5.balance == 1)
      }
      it("rebalances left") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "b", compare)
        val tree3 = AVLTree.insert(tree2, "c", compare)

        assert(tree3.key == "b")
        assert(tree3.height == 2)
        assert(tree3.balance == 0)
      }
      it("rebalances right") {
        val tree1 = AVLTree.insert(Nil, "c", compare)
        val tree2 = AVLTree.insert(tree1, "b", compare)
        val tree3 = AVLTree.insert(tree2, "a", compare)

        assert(tree3.key == "b")
        assert(tree3.height == 2)
        assert(tree3.balance == 0)
      }
      it("balances deep") {
        val tree1 = AVLTree.insert(Nil, "a", compare)
        val tree2 = AVLTree.insert(tree1, "b", compare)
        val tree3 = AVLTree.insert(tree2, "c", compare)
        val tree4 = AVLTree.insert(tree3, "d", compare)
        val tree5 = AVLTree.insert(tree4, "e", compare)
        val tree6 = AVLTree.insert(tree5, "f", compare)
        val tree7 = AVLTree.insert(tree6, "g", compare)

        assert(tree7.key == "d")
        assert(tree7.height == 3)
        assert(tree7.balance == 0)
      }
    }
  }
}