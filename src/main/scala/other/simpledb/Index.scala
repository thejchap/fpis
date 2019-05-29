package simpledb

class Index {
  type KeyValue = (String, Int)
  private var store: AVLTree[KeyValue] = Nil
  private val cmp = (a: KeyValue, b: KeyValue) => {
    if (a._1 > b._1) 1
    else if (a._1 < b._1) -1
    else 0
  }

  def insert(v: KeyValue) = store = AVLTree.insert(store, v, cmp)
  def search(v: KeyValue) = AVLTree.search(store, v, cmp)
}