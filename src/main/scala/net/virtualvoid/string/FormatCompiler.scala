package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode.Bytecode
  import net.virtualvoid.bytecode.ASMCompiler
  import Bytecode._
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
      f.dynMethod(m,classOf[AnyRef])
       .op(compileGetExp(inner,m.getReturnType.asInstanceOf[Class[Object]],retType))
    }
    case ThisExp =>
      f.checkcast(retType) // TODO: don't know why we need this, examine it
    case e:Exp => {
      f.dynMethod(e.method(cl),retType)
    }
  }

  def compileTok[R<:List,LR<:List,T<:java.lang.Object](tok:StrToken,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T]
    = tok match {
      case Literal(str) => f.ldc(str).method2(_.append(_))
      case e:Exp => {
        if (e.returnType(cl).isPrimitive)
          throw new java.lang.Error("currently no primitives can be put out directly. Problematic expression: "+e.identifier)
        
        f.l.load.e
         .op(compileGetExp(e,cl,classOf[AnyRef]))
         .method(_.toString)
         .method2(_.append(_))
      }
      case SpliceExp(exp,sep,inner) => {
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl)).asInstanceOf[Class[AnyRef]]
          val jmpTarget =
            f.l.load.e
             .swap // save one instance of T for later
             .l.load.e
             .op(compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]))
             .method(_.iterator)
             .l.store.e
             .target
          jmpTarget
             .l.load.e
             .method(_.hasNext)
             .ifeq(f=>
               f.l.load.e
                .swap
                .l.load.e
                .method(_.next)
                .checkcast(eleType)
                .l.store.e
                .op(compileToks(inner,eleType))
                .swap
                .dup
                .l.store.e
                .method(_.hasNext)
                .ifeq(f =>
                   f.ldc(sep:jString)
                    .method2(_.append(_))
                    .jmp(jmpTarget)) //todo: introduce ifeq(thenCode,elseTarget)
                .jmp(jmpTarget))
             .swap
             .l.store.e
        }
        else if (retType.isArray){
          val eleType:Class[AnyRef] = retType.getComponentType.asInstanceOf[Class[AnyRef]]

          if (eleType.isPrimitive)
            throw new java.lang.Error("can't handle primitive arrays right now");

          val jmpTarget =
            f.l.load.e
             .swap
             .l.load.e
             .op(compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]))
             .l.store.e
             .bipush(0)
             .target

          jmpTarget
           .dup
           .l.load.e
           .arraylength
           .swap
           .isub
           .ifeq(f =>
             f.dup_x1
              .l.load.e
              .swap
              .aload
              .l.load.e
              .swap
              .l.store.e
              .swap
              .op(compileToks(inner,eleType))
              .swap
              .l.store.e
              .swap
              .bipush(1) // TODO: better use iinc
              .iadd
              .dup
              .l.load.e
              .arraylength
              .isub
              .ifeq(f=>
                 f.swap
                  .ldc(sep)
                  .method2(_.append(_))
                  .swap
                  .jmp(jmpTarget)
              )
              .jmp(jmpTarget)
           )
           .pop
           .swap
           .l.store.e
        }
        else
          throw new java.lang.Error("can only iterate over iterables and arrays right now")
      }
      case Conditional(inner,ifs,thens) => {
        val retType = inner.returnType(cl)
        
        var target:ForwardTarget[R**StringBuilder,LR**T] = f.forwardTarget
        
        f.l.load.e
         .op(f => if (retType.isPrimitive)
               f.op(compileGetExp(inner,cl,classOf[Boolean]))
             else
               f.op(compileGetExp(inner,cl,classOf[java.lang.Boolean])).method(_.booleanValue)              
         )
         .ifeq(_.op(compileToks(ifs,cl)).jmp(target))
         .op(compileToks(thens,cl))
         .targetHere(target)
      }
    }
  def compileToks[R<:List,LR<:List,T<:java.lang.Object](tok:Seq[StrToken],cl:Class[T])(f:F[R**StringBuilder,LR**T]) = {
    var mf:F[R**StringBuilder,LR**T] = f
    for (t<-tok)
      mf = compileTok(t,cl)(f)
    mf
  }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val toks = parser.parse(format)
    ASMCompiler.compile(cl)(
     f =>
       f.l.store.e
         .newInstance(classOf[StringBuilder])
         .op(compileToks(toks,cl))
         .method(_.toString)
     )
  }
}

object FormatCompiler extends IObjectFormatterFactory{
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    def format(o:T):String = Compiler.compile[T](fmt,clazz)(o)
  }
}