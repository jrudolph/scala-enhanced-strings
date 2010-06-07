package net.virtualvoid.string

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{ Transform, TypingTransformers }
import nsc.symtab.Flags

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

    class ESTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
      localTyper = analyzer.newTyper(analyzer.rootContext(unit, EmptyTree, false))
      import unit.error

      val it = ValDef(Modifiers(Flags.PARAM), "it", TypeTree(), EmptyTree)

      def compiled(els: AST.FormatElementList, pos: Position): Tree = {

        def startOf(e: AST.Exp): Position = pos.withPoint(pos.startOrPoint + e.pos.column)
        def compile(els: AST.FormatElementList): Tree = compiled(els, pos)

        def compileParentExpressionInner(inner: AST.Exp, outer: Tree): Tree = inner match {
          case AST.ParentExp(inner, parent) => compileParentExpressionInner(inner, Select(outer, parent))
          case _ => inner match { case AST.Exp(id) => Select(outer, id) }
        }

        def parse(code: String): Tree = {
          val unit = new CompilationUnit(new scala.tools.nsc.util.BatchSourceFile("<snippet>", code))
          val scanner = new syntaxAnalyzer.UnitParser(unit)
          scanner.expr()
        }
        def compileExpression(exp: AST.Exp): Tree = atPos(startOf(exp)) {
          exp match {
            case AST.ThisExp => Ident("it")
            case AST.ParentExp(inner, parent) => compileParentExpressionInner(inner, Ident(parent))
            case AST.ScalaExp(exp) => parse(exp)
            case _ => exp match { case AST.Exp(identifier) => Ident(identifier) }
          }
        }
        def compileElement(el: AST.FormatElement): Tree = el match {
          case AST.Literal(str) => Literal(Constant(str))
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
        }

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

      /** When using <code>postTransform</code>, each node is
       *  visited after its children.
       */
      def postTransform(tree: Tree): Tree = tree match {
        case Literal(Constant(str: String)) =>
          try {
            println(parser.get.Version)
            atPos(tree.pos)(compiled(parser.get.parse(str), tree.pos))
          } catch {
            case p: ParseException => p.printStackTrace; unit.error(tree.pos, p.getMessage); tree
            case e: TypeError => localTyper.reportTypeError(tree.pos, e); tree
          }
        case _ => tree
      }

      val ESType = "EnhancedString".toTypeName
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

      var parser: Option[ESParser] = None

      override def transform(tree: Tree): Tree = tree match {
        case d@DefDef(mods, _, _, _, _, _) =>
          val (newMods, newVersion) = extractVersion(mods)

          newVersion match {
            case Some(v) =>
              val oldParser = parser
              
              parser = ParserFactory.parser(v)
              if (!parser.isDefined)
                error(d.pos, "EnhancedString syntax with version "+v+" not found.")
              
              println("Version now " + v)
              val res = super.transform(tree)
              
              parser = oldParser
              res.asInstanceOf[DefDef].copy(mods = newMods)
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
