package caraph

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class ParserSpec extends FlatSpec with Matchers {

  "A parser" should "be able to parse standard sexpers" in {
    val parser = new CaraphParser("(foo bar baz)")
    assert(Success(Vector(
      SexpList(Vector(SexpIdent("foo"), SexpIdent("bar"), SexpIdent("baz")))))
      === parser.Sexp.run())
  }

  "A parser" should "be able to parse nested sexpers" in {
    val parser = new CaraphParser("((1 2 3) foo (\"bar\"))")
    val innerSexp =
      SexpList(Vector(SexpNumber("1"), SexpNumber("2"), SexpNumber("3")))
    val secondInner = SexpList(Vector(SexpString("bar")))
    assert(
      Success(
        Vector(SexpList(Vector(innerSexp, SexpIdent("foo"), secondInner))))
        === parser.Sexp.run())
  }

  "A parser" should "be able to parse stand alone elements" in {
    val parser = new CaraphParser("foo 1 2 3 \"foo\"")
    assert(
      Success(
        Vector(SexpIdent("foo"),
               SexpNumber("1"),
               SexpNumber("2"),
               SexpNumber("3"),
               SexpString("foo")))
        === parser.Sexp.run())
  }

  "A parser" should "be able to parse all elements of a string" in {
    val parser = new CaraphParser(""" "foobarbaz15\u00f8C"  """)
    assert(
      Success(Vector(SexpString("foobarbaz15øC")))
        === parser.Sexp.run())
  }
}
