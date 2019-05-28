package simpledb

class Index {
  type KeyValue = (String, Int)
  private var store: AVLTree[KeyValue] = Nil
  private val cmp = (a: KeyValue, b: KeyValue) => {
    if (a._1 > b._1) 1
    else if (a._1 < b._1) -1
    else 0
  }

  def set(k: String, o: Int) = store = AVLTree.insert(store, (k, o), cmp)
  def get(k: String): Int = 0
}