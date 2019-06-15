import org.scalatest.FunSpec
import fpis.{Option, None, Some}

case class Employee(name: String, department: String, manager: Option[String] = None)

object Employee {
  def lookupByNameNone(name: String): Option[Employee] = {
    None
  }
  def lookupByNameSome(name: String): Option[Employee] = {
    Some(Employee(name, "engineering", Some("martin odersky")))
  }
}

class Ch4Spec extends FunSpec {
  describe("Option") {
    describe("map") {
      it("maps") {
        val res = Employee.lookupByNameNone("hacker") map(_.department)

        assert(res == None)

        val res2 = Employee.lookupByNameSome("hacker") map(_.department)

        assert(res2 == Some("engineering"))
      }
    }
    describe("flatMap") {
      it("flatMaps") {
        val res = Employee.lookupByNameNone("hacker") flatMap(_.manager)

        assert(res == None)

        val res2 = Employee.lookupByNameSome("hacker") flatMap(_.manager)

        assert(res2 == Some("martin odersky"))
      }
    }
    describe("getOrElse") {
      it("gets with a default") {
        val res = Employee.lookupByNameNone("hacker") map(_ name) getOrElse("martin")

        assert(res == "martin")

        val res2 = Employee.lookupByNameSome("hacker") map(_ name) getOrElse("martin")

        assert(res2 == "hacker")
      }
    }
  }
}