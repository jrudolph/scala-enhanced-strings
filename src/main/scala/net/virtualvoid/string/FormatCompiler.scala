package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode.Bytecode
  import net.virtualvoid.bytecode.ASMCompiler
  import Bytecode._
  import Bytecode.Instructions._
  import Bytecode.Implicits._

  val parser = EnhancedStringFormatParser
  import AST._
  
  def elementType(it:java.lang.reflect.Type,of:Class[_])
  	:Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,of,Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }

  def compileGetExp[R<:List,LR<:List,T,Ret](exp:Exp
                                            ,cl:Class[T]
                                            ,retType:Class[Ret])
                                           (f:F[R**T,LR]):F[R**Ret,LR] = 
  exp match {
    case p@ParentExp(inner,parent) =>{
      val m = p.method(cl)
      f ~ invokemethod1Dyn(m,classOf[AnyRef]) ~ 
       compileGetExp(inner,m.getReturnType.asInstanceOf[Class[Object]],retType)
    }
    case ThisExp =>
      f ~ checkcast(retType) // TODO: don't know why we need this, examine it
    case e:Exp => {
      f ~ invokemethod1Dyn(e.method(cl),retType)
    }
  }
    
  def compileFormatElementList[R<:List,LR<:List,T<:java.lang.Object]
                 (elements:FormatElementList,cl:Class[T])
                 (f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T] =
    elements.elements.foldLeft(f){(frame,element) => 
      compileElement(element,cl)(frame)}

  def id[X]:X=>X = x=>x
  
  def compileElement[R<:List,LR<:List,T<:java.lang.Object]
                     (ele:FormatElement,cl:Class[T])
                     (f:F[R**StringBuilder,LR**T])
                     :F[R**StringBuilder,LR**T]
    = ele match {
      case Literal(str) => 
        f ~ ldc(str) ~ invokemethod2(_.append(_))
      case ToStringConversion(e) =>
        f ~ local[_0,T].load() ~
          compileGetExp(e,cl,classOf[AnyRef]) ~ 
          invokemethod1(_.toString) ~ 
          invokemethod2(_.append(_))
      case Expand(exp,sep,inner) => {
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl)
                                                  ,classOf[java.lang.Iterable[_]])
                                                  .asInstanceOf[Class[AnyRef]]
          val jmpTarget =
            f ~ 
             local[_0,T].load() ~
             swap ~ // save one instance of T for later
             local[_0,T].load() ~
             compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
             invokemethod1(_.iterator) ~
             local[_0,java.util.Iterator[AnyRef]].store() ~
             target
          
          jmpTarget ~
             local[_0,java.util.Iterator[AnyRef]].load() ~
             invokemethod1(_.hasNext) ~
             ifeq(f =>
               f ~
                local[_0,java.util.Iterator[AnyRef]].load() ~
                swap ~
                local[_0,java.util.Iterator[AnyRef]].load() ~
                invokemethod1(_.next) ~
                checkcast(eleType) ~
                local[_0,AnyRef].store() ~
                compileFormatElementList(inner,eleType) ~
                swap ~
                dup ~
                local[_0,java.util.Iterator[AnyRef]].store() ~
                invokemethod1(_.hasNext) ~
                ifeq(f =>
                   f~ldc(sep:jString) ~
                    invokemethod2(_.append(_)) ~
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
            local[_0,ST].store() ~ id
          
          f ~  //sb | o
            local[_0,T].load() ~ // sb,o
            dup ~ //sb,o,o
            compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]) ~
            newInstance(classOf[StringBuilder]) ~
            foldArray( // sb,o,index,sb,ele | array
              // add separator if nothing was added yet 
              _ ~
                swap ~
                dup ~
                invokemethod1(_.length) ~
                ifeq2(
                  f=>f,
                  _ ~ ldc(sep) ~ invokemethod2(_.append(_))
                ) ~
                swap ~ // sb,o,index,sb,ele | array

                // format element
                swapTopWithLocal0 ~ //sb,o,index,sb,array | ele
                swap ~
                compileFormatElementList(inner,eleType) ~ 
                swap ~
                local[_0,Array[AnyRef]].store() ~ id// sb,o,index,sb | array
            ) ~ // sb,o,sb | array
            swap ~ // sb,sb,o | array
            local[_0,T].store() ~
            invokemethod2(_.append(_))
        }
        else
          throw new java.lang.Error("can only iterate over "+
                                      "iterables and arrays right now")
      }
      case Conditional(inner,thens,elses) => {
        val retType = inner.returnType(cl)

        if (retType == java.lang.Boolean.TYPE || 
              classOf[java.lang.Boolean].isAssignableFrom(retType)){
          f ~ 
            local[_0,T].load() ~
            (if (retType == java.lang.Boolean.TYPE)
               compileGetExp(inner,cl,classOf[Boolean])
             else 
               compileGetExp(inner,cl,classOf[java.lang.Boolean]) _ 
             ~ invokemethod1(_.booleanValue)
            ) ~
            ifeq2(
              compileFormatElementList(elses,cl),
              compileFormatElementList(thens,cl))
        }
        else if (classOf[Option[AnyRef]].isAssignableFrom(retType)){
          val eleType = elementType(inner.genericReturnType(cl)
                                    ,classOf[Option[_]])
            .asInstanceOf[Class[AnyRef]]
          f ~
            local[_0,T].load() ~
            compileGetExp(inner,cl,classOf[Option[AnyRef]]) ~
            dup ~
            invokemethod1(_.isDefined) ~
            ifeq2(
              _ ~ pop ~ compileFormatElementList(elses,cl),
              _ ~ 
                checkcast(classOf[Some[AnyRef]]) ~
                invokemethod1(_.get) ~
                local[_0,T].load() ~
                swap ~
                local[_0,AnyRef].store() ~
                swap ~
                compileFormatElementList(thens,eleType) ~
                swap ~
                local[_0,T].store[R**StringBuilder,LR**AnyRef]())
        }
        else
          throw new Error("can't use "+retType+" in a conditional")
      }
      case DateConversion(exp,format) => {
        val retType = exp.returnType(cl)
        
        val DateClass:Class[java.util.Date] = classOf[java.util.Date]
        val CalendarClass:Class[java.util.Calendar] = classOf[java.util.Calendar]
        
        f ~ newInstance(classOf[java.text.SimpleDateFormat]) ~
          dup ~
          ldc(format) ~ 
          invokemethod2(_.applyPattern(_)) ~ pop_unit ~
          local[_0,T].load() ~
          (f => 
            retType match {
              case x if DateClass.isAssignableFrom(x)     => 
                f ~ compileGetExp(exp,cl,DateClass)
              case x if CalendarClass.isAssignableFrom(x) => 
                f ~                
                  compileGetExp(exp,cl,CalendarClass) ~ 
                  invokemethod1(_.getTime)
              case _ => throw new java.lang.Error(
                "Expected date- or calendar- typed property. "+
                cl+" can't be converted.") 
            }
          ) ~
          invokemethod2(_.format(_)) ~
          invokemethod2(_.append(_))
      }
    }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val elements:FormatElementList = parser.parse(format)
    ASMCompiler.compile(cl)(
      _ 
      ~ local[_0,T].store()
      ~ newInstance(classOf[StringBuilder])
      ~ compileFormatElementList(elements,cl)
      ~ invokemethod1(_.toString)
     )
  }
}

object FormatCompiler extends IObjectFormatterFactory{
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String) = 
	  new IObjectFormatter[T]{
	  	val compiler = Compiler.compile[T](fmt,clazz)
	  	def format(o:T):String = compiler(o)
  	  }
}