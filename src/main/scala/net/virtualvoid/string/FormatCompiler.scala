package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode.Bytecode
  import net.virtualvoid.bytecode.ASMCompiler
  import Bytecode._
  import Bytecode.Operations._
  import Bytecode.Implicits._

  val parser = EnhancedStringFormatParser
  import parser.lexical._

  def elementType(it:java.lang.reflect.Type):Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,classOf[java.lang.Iterable[_]],Array()) match{
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

  def compileTok[R<:List,LR<:List,T<:java.lang.Object](tok:StrToken,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T]
    = tok match {
      case StrTokens(toks) => {
        var mf:F[R**StringBuilder,LR**T] = f
        for (t<-toks)
          mf = compileTok(t,cl)(f)
        mf
      }
      case Literal(str) => f~ldc(str)~method2(_.append(_))
      case e:Exp =>
        f ~ load(l0) ~ 
          compileGetExp(e,cl,classOf[AnyRef]) ~ 
          method(_.toString) ~ 
          method2(_.append(_))
      case SpliceExp(exp,sep,inner) => {
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl)).asInstanceOf[Class[AnyRef]]
          val jmpTarget =
            f ~ load(l0) ~
             swap ~ // save one instance of T for later
             load(l0) ~
             compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
             method(_.iterator) ~
             (_.l.store.e) ~
             target
          jmpTarget ~
             load(l0) ~
             method(_.hasNext) ~
             ifeq(f =>
               f.l.load.e ~
                swap ~
                load(l0) ~
                method(_.next) ~
                checkcast(eleType) ~
                (_.l.store.e) ~
                compileTok(inner,eleType) ~
                swap ~
                dup ~
                (_.l.store.e) ~
                method(_.hasNext) ~
                ifeq(f =>
                   f~ldc(sep:jString) ~
                    method2(_.append(_)) ~
                    jmp(jmpTarget)) ~ //todo: introduce ifeq(thenCode,elseTarget)
                jmp(jmpTarget)) ~
             swap ~
             (_.l.store.e)
        }
        else if (retType.isArray){
          val eleType:Class[AnyRef] = retType.getComponentType.asInstanceOf[Class[AnyRef]]

          if (eleType.isPrimitive)
            throw new java.lang.Error("can't handle primitive arrays right now");

          val jmpTarget =
            f ~ 
             load(l0) ~
             swap ~
             load(l0) ~
             compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]) ~
             (_.l.store.e) ~
             bipush(0) ~
             target

          jmpTarget ~ 
           dup ~
           load(l0) ~
           arraylength ~
           swap ~
           isub ~
           ifeq(f =>
             f~dup_x1 ~
              load(l0) ~
              swap ~
              aload ~
              load(l0) ~
              swap ~
              (_.l.store.e) ~
              swap ~
              compileTok(inner,eleType) ~
              swap ~
              (_.l.store.e) ~
              swap ~
              bipush(1) ~ // TODO: better use iinc
              iadd ~
              dup ~
              load(l0) ~
              arraylength ~
              isub ~
              ifeq(f =>
                 f~swap ~
                  ldc(sep) ~
                  method2(_.append(_)) ~
                  swap ~
                  jmp(jmpTarget)
              ) ~
              jmp(jmpTarget)
           ) ~
           pop ~
           swap ~
           (_.l.store.e)
        }
        else
          throw new java.lang.Error("can only iterate over iterables and arrays right now")
      }
      case Conditional(inner,ifs,thens) => {
        val retType = inner.returnType(cl)
        
        val target:ForwardTarget[R**StringBuilder,LR**T] = f.forwardTarget
        
        f ~ load(l0) ~
         (f => if (retType.isPrimitive)
               f ~ compileGetExp(inner,cl,classOf[Boolean])
             else
               f ~ compileGetExp(inner,cl,classOf[java.lang.Boolean]) ~ method(_.booleanValue)              
         ) ~
         ifeq(_ ~ compileTok(ifs,cl) ~ jmp(target)) ~
         compileTok(thens,cl) ~
         targetHere(target)
      }
      case DateConversion(exp,format) => {
        val retType = exp.returnType(cl)
        
        f ~ newInstance(classOf[java.text.SimpleDateFormat]) ~
          dup ~
          ldc(format) ~ 
          method2(_.applyPattern(_)) ~ pop_unit ~
          load(l0) ~
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
       f ~ (_.l.store.e) ~
         newInstance(classOf[StringBuilder]) ~
         compileTok(toks,cl) ~
         method(_.toString)
     )
  }
}

object FormatCompiler extends IObjectFormatterFactory{
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    def format(o:T):String = Compiler.compile[T](fmt,clazz)(o)
  }
}