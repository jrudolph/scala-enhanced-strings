/* SeS - Scala Enhanced Strings
* Copyright 2008 - 2010 Johannes Rudolph
*/

package net.virtualvoid.string

import scala.util.parsing.input.Positional

object AST{
  trait FormatElement extends Positional {
    def format(o:AnyRef):String
  }
  case class Literal(str:String) extends FormatElement{
    def chars = str
    def format(o:AnyRef):String = str
  }
  case class ToStringConversion(exp:Exp) extends FormatElement{
    def format(o:AnyRef):String = exp.eval(o).toString
  }
  case class Conditional(condition:Exp,thenToks:FormatElementList,elseToks:FormatElementList) extends FormatElement{
    def chars =""
    def format(o:AnyRef) = condition.eval(o) match {
      case java.lang.Boolean.TRUE => thenToks.format(o)
      case java.lang.Boolean.FALSE => elseToks.format(o)
      case x: Option[_] => x.asInstanceOf[Option[AnyRef]].map(thenToks.format).getOrElse(elseToks.format(o))
    }
  }
  case class DateConversion(exp:Exp,format:String) extends FormatElement{
    val df = new java.text.SimpleDateFormat(format)
    def format(o:AnyRef) = df.format(exp.eval(o) match {
      case cal:java.util.Calendar => cal.getTime
      case date:java.util.Date => date
    })
    def chars = ""
  }
  case class Expand(exp:Exp,sep:String,inner:FormatElementList) extends FormatElement{
    //def chars = exp.chars + ":" + sep
    def realEval(l:Iterable[AnyRef]):String = l.map(inner.format(_)) mkString sep
    import Java.it2it
    def format(o:AnyRef) = exp.eval(o) match{
      // array or collection or similar
      case l: java.lang.Iterable[_] => realEval(l.asInstanceOf[java.lang.Iterable[AnyRef]])
      case l: Iterable[_] => realEval(l.asInstanceOf[Iterable[AnyRef]])
      case a: Array[AnyRef] => realEval(a)
    }
  }

  trait Exp extends Positional {
    def eval(o: AnyRef): AnyRef
  }
  case class Ident(identifier:String) extends Exp {
    def chars = identifier

    import java.lang.reflect.{Method}
    import java.lang.NoSuchMethodException
    def findMethod(c:Class[_],name:String):Option[Method] =
      try {
        val res = c.getMethod(name)
        res.setAccessible(true)
        Some(res)
      }catch{
        case e:NoSuchMethodException => None
      }
    def realmethod(cl:Class[_]):Method =
      Array("get"+capitalize(identifier),identifier)
        .flatMap(findMethod(cl,_).toList).headOption
        .getOrElse(throw new java.lang.Error("couldn't find method " + identifier + " in class "+cl.getName+" methods: "+cl.getMethods.map(_.getName).mkString(", ")))
    var m:Method = null
    def method(cl:Class[_]) = {
      if (m == null){
        m = realmethod(cl)
      }
      m
    }
    def returnType(callingCl:Class[_]):Class[_] = method(callingCl).getReturnType
    def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type = method(callingCl).getGenericReturnType
    def capitalize(s:String):String = s.substring(0,1).toUpperCase + s.substring(1)
    def eval(o:AnyRef) = method(o.getClass).invoke(o)
  }
  case class ScalaExp(exp: String) extends Exp {
    override def eval(o: AnyRef) = throw new UnsupportedOperationException("not supported in interpreter yet")
  }
  case object ThisExp extends Exp {
    override def eval(o:AnyRef) = o
    def returnType(callingCl:Class[_]):Class[_] = callingCl
    def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type =
      throw new java.lang.Error("No generic type information available for "+callingCl.getName+
        " since it is erased. #this can't be used in conditional or expand expressions")
  }
  case class ParentExp(inner:Exp, parent: String) extends Exp {
    override def eval(o:AnyRef) = inner.eval(Ident(parent).eval(o))
  }

  case class FormatElementList(elements:Seq[FormatElement]){
    def format(o:AnyRef):String = elements.map(_.format(o)) mkString ""
  }
}

