package net.virtualvoid.string

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform

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
    
    class ESTransformer extends /*Typing*/ Transformer {
	    /** When using <code>preTransform</code>, each node is
	     *  visited before its children.
	     */
	    def preTransform(tree: Tree): Tree = tree match {
	      case _ => tree
	    }
     
	    def compileExpression(exp:AST.Exp):Tree = exp match{
	      case AST.ThisExp => This("")
	      //case AST.ParentExp(inner,parent) => This("")
	      case AST.Exp(identifier) => Ident(identifier)
	    }
	    def compileElement(el:AST.FormatElement):Tree = el match{
	      case AST.Literal(str) => Literal(Constant(str))
	      case AST.ToStringConversion(exp) => compileExpression(exp)
	    }
	    def compile(els:AST.FormatElementList,tree:Tree):Tree =
	    	if (els.elements.size == 1)
	    		compileElement(els.elements(0))
	    	else
	    		tree//Literal(Constant("long format"))
     
	    /** When using <code>postTransform</code>, each node is
	     *  visited after its children.
	     */
	    def postTransform(tree: Tree): Tree = tree match {
	      case This(qual) => {System.out.println(tree+":"+qual+qual.getClass+":"+qual.toString.length);tree}
	      case Literal(Constant(str:String)) =>
	          compile(EnhancedStringFormatParser.parse(str),tree)
	      case _ => tree
	    }
	
	    override def transform(tree: Tree): Tree = {
	      postTransform(super.transform(preTransform(tree)))
	    }
    }
  }
}
