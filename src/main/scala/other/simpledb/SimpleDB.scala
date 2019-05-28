package simpledb

import scala.collection.mutable.Map

class SimpleDB {
  private val index = new Index()
  private val db: Map[String, String] = Map()

  def set(k: String, v: String): Unit = {
    index.insert((k, v))
    db(k) = v
  }

  def get(k: String): String = db(k)
}