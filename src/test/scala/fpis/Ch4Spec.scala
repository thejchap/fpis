import org.scalatest.FunSpec
import fpis.{Option, None, Some, Ch4}

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
    describe("orElse") {
      it("gets with a fallback to a 2nd option") {
        val res = Employee.lookupByNameNone("hacker") orElse(Employee.lookupByNameSome("tenderlove"))

        assert(res.map(_ name) == Some("tenderlove"))

        val res2 = Employee.lookupByNameSome("hacker") orElse(Employee.lookupByNameSome("tenderlove"))

        assert(res2.map(_ name) == Some("hacker"))
      }
    }
    describe("filter") {
      it("returns none if the filter doesnt match") {
        val res = Employee.lookupByNameSome("hacker") filter(_.name == "tenderlove")

        assert(res == None)

        val res2 = Employee.lookupByNameSome("hacker") filter(_.name == "hacker")

        assert(res2 == Some(Employee("hacker", "engineering", Some("martin odersky"))))
      }
    }
    describe("Exercise 4.2 - variance") {
      it("returns variance of a sequence of doubles") {
        val seq = Seq(1,0, 3.0)
        val res = Ch4.variance(seq)
        val expected = Some(1.5555555555555556)
        assert(res == expected)
      }
    }
    describe("Exercise 4.3 - map2") {
      it("combines option values") {
        val quote = Ch4.parseInsuranceRateQuote("1", "2")

        assert(quote == Some(2.0))
      }
    }
    describe("Exercise 4.4 - sequence") {
      it("returns none if any item is none") {
        val list = List(Some(1), Some(2), Some(3))
        val res = Ch4.sequence(list)

        assert(res == Some(List(1, 2, 3)))

        val list2 = List(Some(1), None, Some(3))
        val res2 = Ch4.sequence(list2)

        assert(res2 == None)
      }
    }
    describe("Exercise 4.5 - traverse") {
      it("returns the list if the fn evaluates for all") {
        val l = List("1", "2", "3")
        val fn = (x: String) => Some(x.toInt)
        val res = Ch4.traverse(l)(fn)
        val expected = Some(List(1, 2, 3))

        assert(res == expected)

        val l2 = List("1", "2", "Q")
        val res2 = Ch4.traverse(l2)(fn)
        val expected2 = None

        assert(res2 == expected2)
      }
    }
  }
}