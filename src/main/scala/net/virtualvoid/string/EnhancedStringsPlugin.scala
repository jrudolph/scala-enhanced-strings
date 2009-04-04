package net.virtualvoid.string

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform
import nsc.symtab.Flags

class EnhancedStringsPlugin(val global: Global) extends Plugin {
  import global._

  val name = "enhanced-strings"
  val description = "allows variable interpolation in strings"
  val components = List[PluginComponent](Component)
  
  private object Component extends PluginComponent with Transform{
	   import global._
	   import global.definitions._

	   val global = EnhancedStringsPlugin.this.global
	   val runsAfter = "parser"
	  /** The phase name of the compiler plugin
	   *  @todo Adapt to specific plugin.
	   */
	  val phaseName = "enhanced-strings"
	
	  def newTransformer(unit: CompilationUnit) = new ESTransformer
    
    class ESTransformer extends Transformer {
		val it = ValDef(Modifiers(Flags.PARAM), "it", TypeTree(), EmptyTree)
      
	    /** When using <code>preTransform</code>, each node is
	     *  visited before its children.
	     */
	    def preTransform(tree: Tree): Tree = tree match {
	      case _ => tree
	    }
	    def compileParentExpressionInner(inner:AST.Exp,outer:Tree):Tree = inner match {
	      case AST.ParentExp(inner,parent) => compileParentExpressionInner(inner,Select(outer,parent))
	      case _ => inner match {case AST.Exp(id) => Select(outer,id)}
	    }
     
	    def compileExpression(exp:AST.Exp):Tree = exp match{
	      case AST.ThisExp => This("")
	      case AST.ParentExp(inner,parent) => compileParentExpressionInner(inner,Ident(parent))
	      case _ => exp match {case AST.Exp(identifier) => Ident(identifier)}
	    }
	    def compileElement(el:AST.FormatElement):Tree = el match{
	      case AST.Literal(str) => Literal(Constant(str))
	      case AST.ToStringConversion(exp) => Select(compileExpression(exp),"toString")
	      case AST.Expand(exp,sep,inner) => Apply(
	    		  							   Select(
	    		  							     Apply(Select(compileExpression(exp),"map")
	    		  							    		 ,List(Function(List(it),compile(inner)))),"mkString")
                                               ,List(Literal(Constant(sep))))
	      case AST.Conditional(cond,thenEls,elseEls) =>
	        Match(compileExpression(cond),List(
	          CaseDef(Apply(Ident("Some"),List(Bind("it",Ident("_")))),compile(thenEls)),
	          CaseDef(Ident("None"),compile(elseEls))
	        ))
	    }
	    def compile(els:AST.FormatElementList):Tree =
	    	els.elements.size match {
	    	  case 0 => Literal(Constant(""))
	    	  case 1 => compileElement(els.elements(0))
	    	  case _ => els.elements.map(compileElement _).reduceLeft((a,b)=>Apply(Select(a,"$plus"),List(b)))
	    	}
     
	    /** When using <code>postTransform</code>, each node is
	     *  visited after its children.
	     */
	    def postTransform(tree: Tree): Tree = tree match {
	      case This(qual) => {System.out.println(tree+":"+qual+qual.getClass+":"+qual.toString.length);tree}
	      case Literal(Constant(str:String)) =>
	          compile(EnhancedStringFormatParser.parse(str))
	      case _ => tree
	    }
	
	    override def transform(tree: Tree): Tree = {
	      postTransform(super.transform(preTransform(tree)))
	    }
    }
  }
}
