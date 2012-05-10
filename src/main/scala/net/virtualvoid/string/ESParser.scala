/* SeS - Scala Enhanced Strings
* Copyright 2008 - 2010 Johannes Rudolph
*/

package net.virtualvoid.string

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._

object Java{
  implicit def it2it[T](it:java.lang.Iterable[T]):Iterable[T] = new Iterable[T] {
    def iterator = new Iterator[T] {
      val innerIt = it.iterator
      def hasNext = innerIt.hasNext
      def next = innerIt.next
    }
  }
}

class ParseException(msg:String) extends Exception(msg)

// syntax flavor name and version
case class VersionInfo(flavor: String, version: Int)

trait ESParser {
  def parse(input:String): AST.FormatElementList
  def Version: VersionInfo
}

object ParserFactory {
  val defaultVersion: VersionInfo = VersionInfo("poros", 1)
  def parser(info: VersionInfo = defaultVersion): Option[ESParser] = info match {
    case EnhancedStringFormatParser.Version => Some(EnhancedStringFormatParser)
    case _ => None
  }
}

object EnhancedStringFormatParser extends RegexParsers with ESParser {
  val Version = VersionInfo("poros", 1)

  import AST._

  override type Elem = Char
  type Tokens = FormatElement

  implicit def extendParser[T](x:Parser[T]):EParser[T] = EParser[T](x)

  def escaped(char:String):Parser[String] = "#" ~ char ^^ (x=>char)

  val expStartChar = '#'

  def char = "[^#\\]|\\[]".r | escaped("[") | escaped("]") | escaped("#") | escaped("|")
  def chars: Parser[String] = char ~ rep(char) ^^ {case first ~ rest => first :: rest reduceLeft (_+_)}

  def idChar = "\\w".r
  def lit:Parser[Literal] = positioned(chars ^^ {case str => Literal(str)})

  def idPart:Parser[String] = idChar ~ rep(idChar) ^^ {case first ~ rest => first :: rest mkString ""}
  def id:Parser[Exp] = positioned(
    "this" 					^^ {str => ThisExp} |
      idPart ~ opt("." ~> id) ^^ {case str ~ Some(inner) => ParentExp(inner, str)
      case str ~ None => Ident(str)})

  def endOrChars: Parser[String] = not(literal("}}")) ~ char ^^ { case x ~ ch => "" + ch }
  def scalaExpBody: Parser[ScalaExp] = endOrChars ~ rep(endOrChars) ^^ { case first ~ rest => ScalaExp(first :: rest mkString "") }
  def scalaExp: Parser[ScalaExp] = literal("{{") ~!> positioned(scalaExpBody) <~! literal("}}")

  def exp: Parser[Exp] = expStartChar ~>
    (scalaExp | id | extendParser("{") ~!> id <~! "}")

  def expAsString:Parser[FormatElement] = exp ^^ {case exp => ToStringConversion(exp)}

  def sepChars = "[^}]*".r
  def expand = exp ~ opt(inners) ~ opt(extendParser('{') ~!> sepChars <~! '}') <~ "*" ^^ {case exp ~ x ~ separator => Expand(exp,separator.getOrElse(""),x.getOrElse(FormatElementList(List(ToStringConversion(ThisExp)))))}

  def dateConversion:Parser[String] = extendParser("->date[") ~!> "[^\\]]*".r <~ "]"
  def conversion = exp ~ dateConversion ^^ {case exp ~ format => DateConversion(exp,format)}

  def optionalElseClause: Parser[FormatElementList] =
    opt("|" ~> tokens) ^^ (_.getOrElse(FormatElementList(Seq(Literal("")))))

  def clauses = extendParser("?[") ~!>
    (tokens ~ optionalElseClause <~ "]")

  def conditional = exp ~ clauses ^^ {case exp ~ (ifs ~ elses) => Conditional(exp, ifs, elses)}

  def innerExp:Parser[FormatElement] = (expand
    |  conversion
    |  conditional
    |  expAsString
    |  lit)
  def inners = '[' ~> tokens <~ ']'

  def tokens:Parser[FormatElementList] = rep(innerExp) ^^ {case toks => FormatElementList(toks)}

  override val skipWhitespace = false

  case class EParser[T](oldThis:Parser[T]){
    def ~!> [U](p: => Parser[U]): Parser[U]
    = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield b).named("~!>") }

    def <~! [U](p: => Parser[U]): Parser[T]
    = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield a).named("<~!") }
  }

  def parse(input:String):FormatElementList =
    phrase(tokens)(new scala.util.parsing.input.CharArrayReader(input.toCharArray)) match {
      case Success(res,_) => res
      case x:NoSuccess => throw new ParseException(x.msg)
    }
}
