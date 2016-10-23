package caraph

import scala.annotation.switch
import org.parboiled2.CharPredicate.{Digit, Digit19, HexDigit, AlphaNum}
import org.parboiled2._

sealed trait Sexp
case class SexpList(elements: Vector[Sexp]) extends Sexp
case class SexpString(value: String) extends Sexp
case class SexpNumber(value: String) extends Sexp
case class SexpInt(value: Int) extends Sexp
case class SexpIdent(value: String) extends Sexp

object SexpList {
  def apply(elements: Sexp*): SexpList = SexpList(elements.toVector)
}

object CaraphParser {
  val SymbolChar: String = "!_@#$%^&*{}][-_`~:;\\|"
  val WhiteSpaceChar: CharPredicate = CharPredicate(" \n\r\t\f")
  val QuoteBackslash: CharPredicate = CharPredicate("\"\\")
  val QuoteSlashBackSlash: CharPredicate = QuoteBackslash ++ "/"
}

/**
  * Provides a parser and AST for s-expressions.
  *
  * Sexpression ::= ( Value )
  * Value ::=  Symbol | Sexpression | String | Number
  * Symbol ::= a-zA-Z0-9!_@#$%^&*{}][-_`~:;\\|
  * String ::= "<anything>"
  * Number ::= <number>
  *
  * @param input
  */
class CaraphParser(val input: ParserInput) extends Parser with StringBuilding {

  // scalastyle:off public.methods.have.type
  def Sexp = rule { oneOrMore(Expression) ~ EOI }

  def Expression = rule { WhiteSpace ~ Value }

  def Value: Rule1[Sexp] = rule {
    run {
      (cursorChar: @switch) match {
        case '"' => SexpStringRule
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' =>
          SexpNumberRule
        case '(' => SexpListRule
        case x
            if (Character.isLetterOrDigit(x) ||
              CaraphParser.SymbolChar.contains(x)) =>
          SexpIdentRule
        case _ => MISMATCH
      }
    }
  }
  def SexpStringRule = rule { SexpStringUnwrapped ~> (SexpString(_)) }

  def SexpStringUnwrapped = rule {
    '"' ~ clearSB() ~ Characters ~ ws('"') ~ push(sb.toString)
  }

  def SexpNumberRule = rule {
    capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (SexpNumber(_)) ~ WhiteSpace
  }

  def SexpListRule = rule {
    ws('(') ~ zeroOrMore(Value) ~ ws(')') ~> (SexpList(_: _*))
  }

  def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  def NormalChar = rule { !CaraphParser.QuoteBackslash ~ ANY ~ appendSB() }

  def EscapedChar = rule(
    CaraphParser.QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code =>
        sb.append(code.asInstanceOf[Char]); ()
      }
  )

  def Unicode = rule {
    'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer
      .parseInt(_, 16))
  }

  def Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }

  def Digits = rule { oneOrMore(Digit) }

  def Frac = rule { "." ~ Digits }

  def Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }

  def IdentChar = rule { AlphaNum | CharPredicate(CaraphParser.SymbolChar) }

  def SexpIdentRule = rule {
    WhiteSpace ~ clearSB() ~ capture(oneOrMore(IdentChar)) ~>
      (SexpIdent(_)) ~ WhiteSpace
  }

  def WhiteSpace = rule { zeroOrMore(CaraphParser.WhiteSpaceChar) }

  def ws(c: Char) = rule { c ~ WhiteSpace }
  // scalastyle:on public.methods.have.type
}
