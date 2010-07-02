/* SeS - Scala Enhanced Strings
 * Copyright 2008 - 2010 Johannes Rudolph
 */

package net.virtualvoid.string

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{ Transform, TypingTransformers }
import nsc.symtab.Flags

final class OriginalString(final val string: String) extends StaticAnnotation {}

class EnhancedStringsPlugin(val global: Global) extends Plugin {
  import global._

  val name = "enhanced-strings"
  val description = "allows variable interpolation in strings"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent with Transform with TypingTransformers {
    import global._
    import global.definitions._

    val global = EnhancedStringsPlugin.this.global
    override val runsAfter = List("parser")
    /** The phase name of the compiler plugin
     *  @todo Adapt to specific plugin.
     */
    val phaseName = "enhanced-strings"

    def newTransformer(unit: CompilationUnit) = new ESTransformer(unit)

    /** The interface to implement if you want to reparse a string constant
     */
    trait CustomSyntax {
      def parse(code: String, unit: CompilationUnit, pos: Position): Tree
    }

    class ESSyntax(parser: ESParser) extends CustomSyntax {
      def parse(code: String, unit: CompilationUnit, pos: Position): Tree =
	compiled(parser.parse(code), unit, pos)

      def it = ValDef(Modifiers(Flags.PARAM), "it", TypeTree(), EmptyTree)

      def compiled(els: AST.FormatElementList, unit: CompilationUnit, pos: Position): Tree = {
	import unit.error

	import scala.util.parsing.input.Positional
        def startOf(e: Positional): Position = e.pos match {
	  // in case, we don't have a real position, this is only an approximation
	  case scala.util.parsing.input.NoPosition => pos.makeTransparent
	  case _ => pos.withPoint(pos.startOrPoint + e.pos.column)
	}
	def positionOf(e: Positional, length: Int) = e.pos match {
	  case scala.util.parsing.input.NoPosition => pos.makeTransparent	
	  case _ => 
	    val start = pos.startOrPoint + e.pos.column
	    new scala.tools.nsc.util.RangePosition(pos.source, start, start, start + length)
	}
        def compile(els: AST.FormatElementList): Tree = compiled(els, unit, pos)

        def compileParentExpressionInner(inner: AST.Exp, outer: Tree): Tree = inner match {
          case AST.ParentExp(inner, parent) => atPos(positionOf(inner, parent.length))(compileParentExpressionInner(inner, Select(outer, parent)))
          case AST.Ident(id) => atPos(positionOf(inner, id.length))(Select(outer, id))
        }

	def offsetPositionBy(pos: Position, offset: Int) = pos match {
	  case p: scala.tools.nsc.util.RangePosition =>
	    new scala.tools.nsc.util.RangePosition(unit.source, p.start+offset, p.start+offset, p.end+offset)
	  case _ => pos.withSource(unit.source, offset)
	}

        def fixPos(pos: Position, tree: Tree) = {
          object PositionTreeTraverser extends Traverser {
            override def traverse(t: Tree) {
              t.pos = offsetPositionBy(t.pos, pos.point)
              super.traverse(t)
            }
          }
          
          PositionTreeTraverser.traverse(tree)
          tree
        }
        def parse(code: String, pos: Position): Tree = {
          import nsc.util._
          val un = new CompilationUnit(new ScriptSourceFile(unit.source.asInstanceOf[BatchSourceFile], code.toCharArray, pos.startOrPoint))
          val scanner = new syntaxAnalyzer.UnitParser(un)
          scanner.expr()
        }
        def compileExpression(exp: AST.Exp): Tree = atPos(startOf(exp)) {
          exp match {
            case AST.ThisExp => atPos(positionOf(exp, 4))(Ident("it"))
            case AST.ParentExp(inner, parent) => compileParentExpressionInner(inner, atPos(positionOf(exp, parent.length))(Ident(parent)))
            case AST.ScalaExp(scalaExp) => fixPos(startOf(exp), parse(scalaExp, startOf(exp)))
            case AST.Ident(identifier) => atPos(positionOf(exp, identifier.length))(Ident(identifier))
          }
        }
        def compileElement(el: AST.FormatElement): Tree = atPos(startOf(el)){ el match {
          case AST.Literal(str) => atPos(positionOf(el, str.length)) (Literal(Constant(str)))
          case AST.ToStringConversion(exp) => Select(compileExpression(exp), "toString")
          case AST.Expand(exp, sep, inner) => Apply(
            Select(
            Apply(Select(compileExpression(exp), "map")
            , List(Function(List(it), compile(inner)))), "mkString")
            , List(Literal(Constant(sep))))
          case AST.Conditional(cond, thenEls, elseEls) =>
            Match(Typed(compileExpression(cond), Ident("Any".toTypeName)), List(
              CaseDef(Apply(Ident("Some"), List(Bind("it", Ident("_")))), compile(thenEls)),
              CaseDef(Ident("None"), compile(elseEls)),
              CaseDef(Bind("it", Literal(Constant(true))), compile(thenEls)),
              CaseDef(Literal(Constant(false)), compile(elseEls))
              ))
        }}

        els.elements.size match {
          case 0 => Literal(Constant(""))
          case 1 => compileElement(els.elements(0))
          case _ =>
            // the general case:
            // compile into new StringBuilder().append(a).append(b).[...].append(z).toString
            val createInstance: Tree = Apply(Select(New(Ident("StringBuilder".toTypeName)), nme.CONSTRUCTOR), Nil)
            def appendElement(a: Tree, b: Tree) = Apply(Select(a, "append"), List(b))

            val appender = els.elements.map(compileElement _)
                              .foldLeft(createInstance)(appendElement)

            Apply(Select(appender, "toString"), Nil)
        }
      }     
    }

    class ESTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
      localTyper = analyzer.newTyper(analyzer.rootContext(unit, EmptyTree, false))
      import unit.error

      /** Strip off the delimiters of a string constant's position */
      def fixPosition(pos: Position, len: Int): Position = pos match {
	case p: scala.tools.nsc.util.RangePosition => 
	  val start = p.start
	  val end = p.end
	  val lengthWithDelims = end - start
	  val delims = (lengthWithDelims - len) / 2 - 1
	  println("Found delims of total length "+(lengthWithDelims - len))
	  new scala.tools.nsc.util.RangePosition(p.source, start+delims, start+delims, end-delims)
	case _ => pos
      }

      /** When using <code>postTransform</code>, each node is
       *  visited after its children.
       */
      def postTransform(tree: Tree): Tree = tree match {
        case Literal(Constant(str: String)) =>
          try {
            //println(parser.get.Version)
            atPos(tree.pos.makeTransparent)(parser.get.parse(str, unit, fixPosition(tree.pos, str.length)))
          } catch {
            case p: ParseException => p.printStackTrace; unit.error(tree.pos, p.getMessage); tree
            case e: TypeError => localTyper.reportTypeError(tree.pos, e); tree
          }
        case _ => tree
      }

      val ESType = "EnhanceStrings".toTypeName
      val SyntaxParam = "syntax".toTermName
      val VersionParam = "version".toTermName
      
      val annotationMatcher: PartialFunction[Tree, List[Tree]] = { case Apply(Select(New(Ident(ESType)), nme.CONSTRUCTOR), args) => args }
      
      def versionExtractor(cur: VersionInfo, arg: Tree): VersionInfo = arg match {
        case AssignOrNamedArg(Ident(SyntaxParam), Literal(Constant(flavor: String))) => cur.copy(flavor = flavor)
        case AssignOrNamedArg(Ident(SyntaxParam), c@Literal(Constant(flavor))) => error(c.pos, "The "+SyntaxParam+" attribute of "+ESType+" must be a String value"); cur

        case AssignOrNamedArg(Ident(VersionParam), Literal(Constant(version: Int))) => cur.copy(version = version)
        case AssignOrNamedArg(Ident(VersionParam), c@Literal(Constant(version))) => error(c.pos, "The "+VersionParam+" attribute of "+ESType+" must be an integer value"); cur
        
        case _ => error(arg.pos, "Unknown parameter of "+ESType+": "+arg); cur
      }

      def extractVersion(m: Modifiers): (Modifiers, Option[VersionInfo]) = m match {
        case Modifiers(a, b, anns, d) =>
          val (args, rest) = anns.partition(annotationMatcher.isDefinedAt _)

          (Modifiers(a, b, rest, d), args.headOption.map(annotationMatcher).map(args => args.foldLeft(ParserFactory.defaultVersion)(versionExtractor)))
        case _ => (m, None)
      }

      var parser: Option[CustomSyntax] = None

      def withMods(tree: Tree, newMods: Modifiers): Tree = tree match {
	case d: DefDef    => d.copy(mods = newMods)
	case m: ModuleDef => m.copy(mods = newMods)
	case c: ClassDef  => c.copy(mods = newMods)
	case _            => tree
      }
      override def transform(tree: Tree): Tree = tree match {
        case m: MemberDef =>
          val (newMods, newVersion) = extractVersion(m.mods)

          newVersion match {
            case Some(v) =>
              val oldParser = parser
              
              parser = ParserFactory.parser(v).map(new ESSyntax(_))
              if (!parser.isDefined)
                error(tree.pos, "EnhancedString syntax with version "+v+" not found.")
              
              println("Version now " + v)
              val res = super.transform(tree)
              
              parser = oldParser
              atPos(tree.pos)(withMods(res, newMods))
            case None =>
              super.transform(tree)
          }
        case Apply(Select(New(Ident(ESType)), nme.CONSTRUCTOR), List(x)) => println(x.getClass); super.transform(tree)
        case _ if parser.isDefined => postTransform(super.transform(tree))
        case _ => super.transform(tree)
      }
    }
  }
}
