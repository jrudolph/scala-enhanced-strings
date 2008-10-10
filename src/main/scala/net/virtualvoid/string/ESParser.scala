package net.virtualvoid.string

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._

object Java{
  implicit def it2it[T](it:java.lang.Iterable[T]):Iterable[T] = new Iterable[T]{
    def elements = new Iterator[T]{
      val innerIt = it.iterator
      def hasNext = innerIt.hasNext
      def next = innerIt.next
    }
  }
}

class StrLexer extends Lexical with RegexParsers{
  override type Elem = Char

  trait StrToken extends Token {
    def eval(o:AnyRef):AnyRef
  }
  case class Literal(str:String) extends StrToken{
    def chars = str
    def eval(o:AnyRef):String = str
  }
  case class Exp(identifier:String) extends StrToken{
    def chars = identifier

    import java.lang.reflect.{Method}
    import java.lang.NoSuchMethodException
    def findMethod(c:Class[_],name:String):Option[Method] =
     try {
       val res = c.getMethod(name,null)
       res.setAccessible(true)
       Some(res)
     }catch{
     case _:NoSuchMethodException => None
     }
    def method(cl:Class[_]):Method = Array("get"+capitalize(identifier),identifier).flatMap(findMethod(cl,_).toList).firstOption.getOrElse(throw new java.lang.Error("couldn't find method" + identifier + "in class "+cl.getName))
    def returnType(callingCl:Class[_]):Class[_] = method(callingCl).getReturnType
    def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type = method(callingCl).getGenericReturnType
    def capitalize(s:String):String = s.substring(0,1).toUpperCase + s.substring(1)
    def eval(o:AnyRef) = method(o.getClass).invoke(o,null)
  }
  case class DateConversion(exp:Exp,format:String) extends StrToken{
    val df = new java.text.SimpleDateFormat(format)
    def eval(o:AnyRef) = df.format(exp.eval(o) match {
      case cal:java.util.Calendar => cal.getTime
      case date:java.util.Date => date
    })
    def chars = ""
  }
  case object ThisExp extends Exp(""){
    override def eval(o:AnyRef) = o
    override def returnType(callingCl:Class[_]):Class[_] = callingCl
    override def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type =
      throw new java.lang.Error("no generic type information available for "+callingCl.getName+" since it is erased")
  }
  case class ParentExp(inner:Exp,parent:String) extends Exp(parent){
    override def eval(o:AnyRef) = inner.eval(super.eval(o))
  }
  case class SpliceExp(exp:Exp,sep:String,inner:List[StrToken]) extends StrToken{
    def chars = exp.chars + ":" + sep
    def realEval(l:Iterable[AnyRef]):String = l.map(e => inner.map(_.eval(e)) mkString "") mkString sep
    import Java.it2it
    def eval(o:AnyRef) = exp.eval(o) match{
      // array or collection or similar
    case l : java.lang.Iterable[AnyRef] => realEval(l)
    case l : Seq[AnyRef] => realEval(l)
    }
  }

  implicit def extendParser[T](x:Parser[T]):EParser[T] = EParser[T](x)

  def escapedByDoubling(char:String):Parser[String] = char ~ char ^^ (x=>char)

  val expStartChar = '#'

  def char = "[^#\\]\\[]".r | escapedByDoubling("[") | escapedByDoubling("]") | escapedByDoubling("#")
  def idChar = "\\w".r
  def lit:Parser[StrToken] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}

  def idPart:Parser[String] = idChar ~ rep(idChar) ^^ {case first ~ rest => first :: rest mkString ""}
  def id:Parser[Exp] =
    "this" 					^^ {str => ThisExp} |
    idPart ~ opt("." ~> id) ^^ {case str ~ Some(inner) => ParentExp(inner,str)
                                case str ~ None => Exp(str)}

  def exp:Parser[Exp] = expStartChar ~>
    (id | extendParser("{") ~!> id <~! "}")

  def sepChars = "[^}]*".r
  def spliceExp = exp ~ opt(inners) ~ opt(extendParser('{') ~!> sepChars <~! '}') <~ "*" ^^ {case exp ~ x ~ separator => SpliceExp(exp,separator.getOrElse(""),x.getOrElse(List(ThisExp)))}
  
  def dateConversion:Parser[String] = extendParser("->date[") ~!> "[^\\]]*".r <~ "]" 
  def conversion = exp ~ dateConversion ^^ {case exp ~ format => DateConversion(exp,format)}

  def innerExp:Parser[StrToken] = spliceExp | conversion | exp | lit 
  def inners = '[' ~> rep(innerExp) <~ ']'

  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token:Parser[Token] = (EofCh ^^^ EOF
     | innerExp )

  override def whitespace = rep('`')
  override def skipWhitespace = false

  case class EParser[T](oldThis:Parser[T]){
    def ~!> [U](p: => Parser[U]): Parser[U]
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield b).named("~!>") }

    def <~! [U](p: => Parser[U]): Parser[T]
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield a).named("<~!") }
  }
}
object EnhancedStringFormatParser extends TokenParsers{
  type Tokens = StrLexer
  override val lexical = new StrLexer
  import lexical._

  def value:Parser[StrToken] = elem("token",_.isInstanceOf[StrToken]) ^^ {case s:StrToken => s}
  def values = rep(value)

  def parse(input:String):List[lexical.StrToken] = {
      val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
      val output = phrase(values)(scanner)
      output.get
    }
}