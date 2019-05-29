package simpledb

import scala.collection.mutable.Map

class SimpleDB {
  type KeyValue = (String, String)
  private val Size = 256
  private val idx = new Index()
  private val db: Array[KeyValue] = Array.ofDim[KeyValue](Size)
  private var offset: Int = 0

  def set(k: String, v: String): Boolean = {
    db(offset) = (k, v)
    idx.insert((k, offset))
    offset += 1
    true
  }

  def get(k: String): Option[String] = idx.search((k, 0)) match {
    case Some(x) => Some(db(x._2)._2)
    case _ => None
  }
}