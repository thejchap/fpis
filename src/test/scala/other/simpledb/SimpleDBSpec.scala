import org.scalatest.FunSpec
import simpledb.SimpleDB

class SimpleDBSpec extends FunSpec {
  describe("SimpleDB") {
    it("sets and gets values") {
      val db = new SimpleDB()
      db.set("name", "jelly")
      val name = db.get("name")

      assert(name == Some("jelly"))
    }
  }
}