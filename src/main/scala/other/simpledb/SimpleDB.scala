package simpledb

import scala.collection.mutable.Map

class SimpleDB {
  private val db: Map[String, String] = Map()
  private var index: AVLTree[Tuple2[String, String]] = Nil
  private val indexCmp = (a: Tuple2[String, String], b: Tuple2[String, String]) => {
    if (a._1 > b._1) 1
    else if (a._1 < b._1) -1
    else 0
  }

  def set(k: String, v: String): Unit = {
    index = AVLTree.insert(index, (k, v), indexCmp)
    db(k) = v
  }

  def get(k: String): String = db(k)
}