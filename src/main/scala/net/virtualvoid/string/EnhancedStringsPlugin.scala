package net.virtualvoid.string

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{Transform, TypingTransformers}
import nsc.symtab.Flags

class EnhancedStringsPlugin(val global: Global) extends Plugin {
  import global._

  val name = "enhanced-strings"
  val description = "allows variable interpolation in strings"
  val components = List[PluginComponent](Component)
  
  private object Component extends PluginComponent with Transform with TypingTransformers{
	   import global._
	   import global.definitions._

	   val global = EnhancedStringsPlugin.this.global
	   override val runsAfter = List("parser")
	  /** The phase name of the compiler plugin
	   *  @todo Adapt to specific plugin.
	   */
	  val phaseName = "enhanced-strings"
	
	  def newTransformer(unit: CompilationUnit) = new ESTransformer(unit)
    
    class ESTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
        localTyper = analyzer.newTyper(analyzer.rootContext(unit, EmptyTree, false))
    
		val it = ValDef(Modifiers(Flags.PARAM), "it", TypeTree(), EmptyTree)
      
	    /** When using <code>preTransform</code>, each node is
	     *  visited before its children.
	     */
	    def preTransform(tree: Tree): Tree = tree match {
	      case _ => tree
	    }
	    def compiled(els:AST.FormatElementList, pos: Position): Tree = {
	    
	      def startOf(e: AST.Exp): Position = pos.withPoint(pos.startOrPoint + e.pos.column)
	      def compile(els: AST.FormatElementList): Tree = compiled(els, pos)
	    
	    def compileParentExpressionInner(inner:AST.Exp,outer:Tree):Tree = inner match {
	      case AST.ParentExp(inner,parent) => compileParentExpressionInner(inner,Select(outer,parent))
	      case _ => inner match {case AST.Exp(id) => Select(outer,id)}
	    }
     
        def parse(code: String): Tree = {
          val unit = new CompilationUnit(new scala.tools.nsc.util.BatchSourceFile("<snippet>", code))
          val scanner = new syntaxAnalyzer.UnitParser(unit)
          scanner.expr()
        } 
	    def compileExpression(exp:AST.Exp):Tree = atPos(startOf(exp)) { exp match {
	      case AST.ThisExp => Ident("it")
	      case AST.ParentExp(inner,parent) => compileParentExpressionInner(inner,Ident(parent))
	      case AST.ScalaExp(exp) => parse(exp)
	      case _ => exp match { case AST.Exp(identifier) => Ident(identifier) }
	    }}
	    def compileElement(el:AST.FormatElement):Tree = el match{
	      case AST.Literal(str) => Literal(Constant(str))
	      case AST.ToStringConversion(exp) => Select(compileExpression(exp),"toString")
	      case AST.Expand(exp,sep,inner) => Apply(
	    		  							   Select(
	    		  							     Apply(Select(compileExpression(exp), "map")
	    		  							    		 ,List(Function(List(it),compile(inner)))),"mkString")
                                               ,List(Literal(Constant(sep))))
	      case AST.Conditional(cond,thenEls,elseEls) =>
	        Match(Typed(compileExpression(cond), Ident("Any".toTypeName)),List(
	          CaseDef(Apply(Ident("Some"),List(Bind("it",Ident("_")))),compile(thenEls)),
	          CaseDef(Ident("None"),compile(elseEls)),
	          CaseDef(Bind("it", Literal(Constant(true))), compile(thenEls)),
	          CaseDef(Literal(Constant(false)), compile(elseEls))
	        ))
	    }
	    
	    	els.elements.size match {
	    	  case 0 => Literal(Constant(""))
	    	  case 1 => compileElement(els.elements(0))
	    	  case _ =>
	    	    // the general case:
	    	    // compile into new StringBuilder().append(a).append(b).[...].append(z).toString
	    	    val appender = els.elements.map(compileElement _)
	    	                      .foldLeft(
	    	                        Apply(Select(New(Ident("StringBuilder".toTypeName)), nme.CONSTRUCTOR), Nil)) { 
	    	                        (a, b) =>
	    	                          Apply(Select(a, "append"), List(b)) 
	    	                        }
	    	    Apply(Select(appender, "toString"), Nil)
	    	}
        }
        
	    /** When using <code>postTransform</code>, each node is
	     *  visited after its children.
	     */
	    def postTransform(tree: Tree): Tree = tree match {
	      case This(qual) => {System.out.println(tree+":"+qual+qual.getClass+":"+qual.toString.length);tree}
	      case Literal(Constant(str:String)) => 
    	      
	        try {
	          atPos(tree.pos)(compiled(EnhancedStringFormatParser.parse(str), tree.pos))
	        } catch {
	          case p:ParseException => p.printStackTrace;unit.error(tree.pos, p.getMessage);tree
	          case e:TypeError => localTyper.reportTypeError(tree.pos, e);tree
	        }
	      case _ => tree
	    }
	
	    override def transform(tree: Tree): Tree = {
	      postTransform(super.transform(preTransform(tree)))
	    }
    }
  }
}
