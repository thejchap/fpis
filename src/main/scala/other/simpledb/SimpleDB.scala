package simpledb

import scala.collection.mutable.Map
import java.io.{FileOutputStream, File}

class SimpleDB {
  private val index = new Index()
  private val db: Map[String, String] = Map()
  private val FileName = "/Users/justinchapman/Desktop/db"

  def set(k: String, v: String): Unit = {
    val offset = writeToFile(k, v)
    index set(k, offset)
    db(k) = v
  }

  def get(k: String): String = {
    db(k)
  }

  private def writeToFile(k: String, v: String): Int = {
    val file = new File(FileName)
    val writer = new FileOutputStream(file, true)
    val bytes = encode(k, v)
    val offset = file.length.toInt
    writer write(bytes)
    writer close()
    offset
  }

  private def encode(k: String, v: String): Array[Byte] = s"$k${v length}$v" getBytes()
}