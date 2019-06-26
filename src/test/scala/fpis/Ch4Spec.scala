import org.scalatest.FunSpec
import fpis.{Option, None, Some, Ch4, Either, Left, Right}

case class Employee(name: String, department: String, manager: Option[String] = None)
case class Employee2(name: String, department: String, manager: Either[String, String] = Left("Not found"))

object Employee {
  def lookupByNameNone(name: String): Option[Employee] = None
  def lookupByNameNone2(name: String): Either[String, Employee2] = Left("Not found")

  def lookupByNameSome(name: String): Option[Employee] =
    Some(Employee(name, "engineering", Some("martin odersky")))

  def lookupByNameSome2(name: String): Either[String, Employee2] =
    Right(Employee2(name, "engineering", Right("martin odersky")))
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
    describe("Exercise 4.6 - Either") {
      it("maps") {
        val res = Employee.lookupByNameNone2("hacker") map(_.department)

        assert(res == Left("Not found"))

        val res2 = Employee.lookupByNameSome2("hacker") map(_.department)

        assert(res2 == Right("engineering"))
      }
      it("flatMaps") {
        val res = Employee.lookupByNameNone2("hacker") flatMap(_.manager)

        assert(res == Left("Not found"))

        val res2 = Employee.lookupByNameSome2("hacker") flatMap(_.manager)

        assert(res2 == Right("martin odersky"))
      }
      it("orElses") {
        val res = Employee.lookupByNameNone2("hacker") orElse(Employee.lookupByNameSome2("tenderlove"))

        assert(res.map(_ name) == Right("tenderlove"))

        val res2 = Employee.lookupByNameSome2("hacker") orElse(Employee.lookupByNameSome2("tenderlove"))

        assert(res2.map(_ name) == Right("hacker"))
      }
      it("map2s") {
        def Try[A](a: => A): Either[Exception, A] =
          try Right(a)
          catch { case e: Exception => Left(e) }

        def parseInsuranceRateQuote(age: String, numTix: String): Either[Exception, Double] = for {
          a <- Try { age toInt }
          t <- Try { numTix toInt }
        } yield Ch4.insuranceRateQuote(a, t)

        val quote = parseInsuranceRateQuote("1", "2")

        assert(quote == Right(2.0))
      }
    }
    describe("Exercise 4.7 - sequence/traverse") {
      it("sequences") {
        val list = List(Right(1), Right(2), Right(3))
        val res = Ch4.sequence2(list)

        assert(res == Right(List(1, 2, 3)))

        val list2 = List(Right(1), Left("test"), Right(3))
        val res2 = Ch4.sequence2(list2)

        assert(res2.isInstanceOf[Left[scala.MatchError]])
      }
      it("traverses") {
        val l = List("1", "2", "3")
        val fn = (x: String) => Right(x.toInt)
        val res = Ch4.traverse2(l)(fn)
        val expected = Right(List(1, 2, 3))

        assert(res == expected)

        val l2 = List("1", "2", "Q")
        val res2 = Ch4.traverse2(l2)(fn)
        assert(res2.isInstanceOf[Left[java.lang.NumberFormatException]])
      }
    }
  }
}