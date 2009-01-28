package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode.Bytecode
  import net.virtualvoid.bytecode.ASMCompiler
  import Bytecode._
  import Bytecode.Operations._
  import Bytecode.Implicits._

  val parser = EnhancedStringFormatParser
  import AST._

  def elementType(it:java.lang.reflect.Type,of:Class[_]):Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,of,Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }

  def compileGetExp[R<:List,LR<:List,T,Ret](exp:Exp,cl:Class[T],retType:Class[Ret])(f:F[R**T,LR]):F[R**Ret,LR] = exp match{
    case p@ParentExp(inner,parent) =>{
      val m = p.method(cl)
      f ~ dynMethod(m,classOf[AnyRef]) ~ 
       compileGetExp(inner,m.getReturnType.asInstanceOf[Class[Object]],retType)
    }
    case ThisExp =>
      f ~ checkcast(retType) // TODO: don't know why we need this, examine it
    case e:Exp => {
      f ~ dynMethod(e.method(cl),retType)
    }
  }
  
  def loop[ST<:List,LT<:List,T](
    cond:F[ST,LT]=>F[ST**Boolean,LT]
    ,next:F[ST,LT]=>F[ST**T,LT])(body:F[ST**T,LT]=>F[ST,LT]):F[ST,LT]=>F[ST,LT] = null
  
  def compileFormatElements[R<:List,LR<:List,T<:java.lang.Object](elements:FormatElements,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T] =
    elements.toks.foldLeft(f){(frame,token) => compileTok(token,cl)(frame)}

  def compileTok[R<:List,LR<:List,T<:java.lang.Object](tok:FormatElement,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T]
    = tok match {
      case Literal(str) => 
        f ~ ldc(str) ~ method2(_.append(_))
      case ToStringConversion(e) =>
        f ~ local[_0,T].load() ~
          compileGetExp(e,cl,classOf[AnyRef]) ~ 
          method(_.toString) ~ 
          method2(_.append(_))
      case Expand(exp,sep,inner) => {
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl),classOf[java.lang.Iterable[_]]).asInstanceOf[Class[AnyRef]]
          val jmpTarget =
            f ~ 
             local[_0,T].load() ~
             swap ~ // save one instance of T for later
             local[_0,T].load() ~
             compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
             method(_.iterator) ~
             local[_0,java.util.Iterator[AnyRef]].store() ~
             target
          
          jmpTarget ~
             local[_0,java.util.Iterator[AnyRef]].load() ~
             method(_.hasNext) ~
             ifeq(f =>
               f ~
                local[_0,java.util.Iterator[AnyRef]].load() ~
                swap ~
                local[_0,java.util.Iterator[AnyRef]].load() ~
                method(_.next) ~
                checkcast(eleType) ~
                local[_0,AnyRef].store() ~
                compileFormatElements(inner,eleType) ~
                swap ~
                dup ~
                local[_0,java.util.Iterator[AnyRef]].store() ~
                method(_.hasNext) ~
                ifeq(f =>
                   f~ldc(sep:jString) ~
                    method2(_.append(_)) ~
                    jmp(jmpTarget)) ~ //todo: introduce ifeq(thenCode,elseTarget)
                jmp(jmpTarget)) ~
             swap ~
             local[_0,T].store[R**StringBuilder,LR**java.util.Iterator[AnyRef]]()
        }
        else if (retType.isArray){
          val eleType:Class[AnyRef] = retType.getComponentType.asInstanceOf[Class[AnyRef]]

          if (eleType.isPrimitive)
            throw new java.lang.Error("can't handle primitive arrays right now");

          import Bytecode.RichOperations.foldArray
          
          def swapTopWithLocal0[S<:List,L<:List,ST,LT]:F[S**ST,L**LT] => F[S**LT,L**ST] = 
            _ ~
            local[_0,LT].load() ~
            swap ~
            local[_0,ST].store[S**LT,L**LT]()
          
          f ~
             local[_0,T].load() ~
             swap ~
             local[_0,T].load() ~
             compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]) ~
             swap ~
             foldArray(// index,sb,ele | array
               _ ~ 
               swapTopWithLocal0 ~ // index,sb,array | ele
               swap ~ // index,array,sb
               compileFormatElements(inner,eleType) ~ //index,array,sb | ele
               swap ~
               local[_0,Array[AnyRef]].store() ~
               // check if it was latest element or not so we can insert separator
               swap ~ // sb,index
               dup_x1 ~ // index,sb,index
               local[_0,Array[AnyRef]].load() ~ // index,sb,index,array
               arraylength ~ // index,sb,index,length
               bipush(1) ~ // index,sb,index,length,1
               isub ~ // index,sb,index,length-1
               isub ~ // index,sb,index-(length-1)
               ifeq2( // index,sb
                 f=>f, //FIXME: use id func here
                 ldc(sep) ~ method2(_.append(_)) // append separator if we are not at the end of the array
               )
             ) ~
             swap ~
             local[_0,T].store[R**StringBuilder,LR**Array[AnyRef]]()
        }
        else
          throw new java.lang.Error("can only iterate over iterables and arrays right now")
      }
      case Conditional(inner,thens,elses) => {
        val retType = inner.returnType(cl)

        if (retType == java.lang.Boolean.TYPE || classOf[java.lang.Boolean].isAssignableFrom(retType)){
          f ~ 
            local[_0,T].load() ~
            (if (retType == java.lang.Boolean.TYPE)
               compileGetExp(inner,cl,classOf[Boolean])
             else 
               compileGetExp(inner,cl,classOf[java.lang.Boolean]) _ ~ method(_.booleanValue)
            ) ~
            ifeq2(
              compileFormatElements(elses,cl),
              compileFormatElements(thens,cl))
        }
        else if (classOf[Option[AnyRef]].isAssignableFrom(retType)){
          val eleType = elementType(inner.genericReturnType(cl),classOf[Option[_]]).asInstanceOf[Class[AnyRef]]
          f ~
            local[_0,T].load() ~
            compileGetExp(inner,cl,classOf[Option[AnyRef]]) ~
            dup ~
            method(_.isDefined) ~
            ifeq2(
              _ ~ pop ~ compileFormatElements(elses,cl),
              _ ~ 
                checkcast(classOf[Some[AnyRef]]) ~
                method(_.get) ~
                local[_0,T].load() ~
                swap ~
                local[_0,AnyRef].store() ~
                swap ~
                compileFormatElements(thens,eleType) ~
                swap ~
                local[_0,T].store[R**StringBuilder,LR**AnyRef]()(replace_0))
        }
        else
          throw new Error("can't use "+retType+" in a conditional")
      }
      case DateConversion(exp,format) => {
        val retType = exp.returnType(cl)
        
        f ~ newInstance(classOf[java.text.SimpleDateFormat]) ~
          dup ~
          ldc(format) ~ 
          method2(_.applyPattern(_)) ~ pop_unit ~
          local[_0,T].load() ~
          (f => 
            if (classOf[java.util.Date].isAssignableFrom(retType))
              f ~ compileGetExp(exp,cl,classOf[java.util.Date])
            else if (classOf[java.util.Calendar].isAssignableFrom(retType))
              f ~ compileGetExp(exp,cl,classOf[java.util.Calendar]) ~ method(_.getTime)
            else
                throw new java.lang.Error("only date or time can be converted")
          ) ~
          method2(_.format(_)) ~
          method2(_.append(_))
      }
    }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val toks = parser.parse(format)
    ASMCompiler.compile(cl)(
     f =>
       f ~ local[_0,T].store() ~
         newInstance(classOf[StringBuilder]) ~
         compileFormatElements(toks,cl) ~
         method(_.toString)
     )
  }
}

object FormatCompiler extends IObjectFormatterFactory{
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val compiler = Compiler.compile[T](fmt,clazz)
    def format(o:T):String = compiler(o)
  }
}