package net.virtualvoid.string

import reflect.makro._
import scala.util.parsing.input.Positional

object EnhancedStringMacro {
  def enhance(c: Context { type PrefixType = WithIP } ): c.Expr[String] = {
    import c.mirror._

    lazy val optionTpe = implicitly[TypeTag[Option[Any]]].tpe
    lazy val booleanTpe = implicitly[TypeTag[Boolean]].tpe

    case class RichPos(p: Position) {
      def trans(f: Position => scala.reflect.api.Position): Position = f(p).asInstanceOf[Position]
    }
    implicit def toRichPos(p: Position): RichPos = RichPos(p)

    def compiled(els: AST.FormatElementList, pos: Position): Expr[String] = {
      def startOf(e: Positional): Position = e.pos match {
        // in case, we don't have a real position, this is only an approximation
        case scala.util.parsing.input.NoPosition => pos.trans(_.makeTransparent)
        case _ => pos.trans(_.withPoint(pos.startOrPoint + e.pos.column))
      }
      def positionOf(e: Positional, length: Int): Position  = e.pos match {
        case scala.util.parsing.input.NoPosition => pos.trans(_.makeTransparent)
        case _ =>
          val start = pos.startOrPoint + e.pos.column
          pos.trans(_.withStart(start).withPoint(start).withEnd(start + length))
      }

      def at[T](pos: Position)(expr: Expr[T]): Expr[T] =
        Expr(atPos(pos)(expr.tree))

      def compile(els: AST.FormatElementList): Expr[String] = compiled(els, pos)
      def compileElement(el: AST.FormatElement): Expr[String] = at(startOf(el)) { el match {
        case AST.Literal(str) =>
          at(positionOf(el, str.length)) {
            c.reify(c.literal(str).eval)
          }

        case AST.ToStringConversion(exp) =>
          c.reify(compileExpression(exp).eval.toString)

        case AST.Expand(exp, sep, inner) =>
          c.reify(
            compileExpression[Traversable[Any]](exp).eval
              .map(it => compile(inner).eval)
              .mkString(c.literal(sep).eval))

        case AST.Conditional(cond, thenEls, elseEls) =>
          val compiledCond = compileExpression(cond)
          val condTpe = c.typeCheck(compiledCond.tree).tpe

          def condExpr[T]: Expr[T] = compiledCond.asInstanceOf[Expr[T]]

          val thenExpr = compile(thenEls)
          val elseExpr = compile(elseEls)

          if (condTpe <:< optionTpe)
            c.reify(condExpr[Option[Any]].eval.fold(elseExpr.eval)(it => thenExpr.eval))
          else if (condTpe =:= booleanTpe)
            c.reify(if (condExpr[Boolean].eval) thenExpr.eval else elseExpr.eval)
          else {
            c.error(pos, "Conditional expression has to be of type Option or Boolean")
            c.reify("<error>")
          }
      }}
      def compileExpression[T](exp: AST.Exp): Expr[T] = /*atPos(startOf(exp))*/ {
        exp match {
          case AST.ThisExp => at(positionOf(exp, 4))(Expr[T](Ident("it")))
          case AST.ParentExp(inner, parent) =>
            compileParentExpressionInner[T](inner, atPos(positionOf(exp, parent.length))(Ident(parent)))

          // parsing not yet supported
          //case AST.ScalaExp(scalaExp) =>
            //fixPos(startOf(exp), parse(scalaExp, startOf(exp)))
          case AST.Ident(identifier) => at(positionOf(exp, identifier.length))(Expr[T](Ident(identifier)))
        }
      }
      def compileParentExpressionInner[T](inner: AST.Exp, outer: Tree): Expr[T] = inner match {
        case AST.ParentExp(inner, parent) =>
          at(positionOf(inner, parent.length))(compileParentExpressionInner(inner, Select(outer, parent)))
        case AST.Ident(id) =>
          at(positionOf(inner, id.length))(Expr(Select(outer, id)))
      }

      els.elements.size match {
        case 0 => c.reify("")
        case 1 => compileElement(els.elements(0))
        case _ =>
          // the general case:
          // compile into new StringBuilder().append(a).append(b).[...].append(z).toString
          val createInstance: Expr[StringBuilder] = c.reify(new StringBuilder())
          def appendElement(a: Expr[StringBuilder], b: Expr[Any]) = c.reify(a.eval.append(b.eval))

          val appender = els.elements.map(compileElement(_))
            .foldLeft(createInstance)(appendElement(_, _))

          c.reify(appender.eval.toString)
        }
    }

    val Apply(_, (lit@c.mirror.Literal(c.mirror.Constant(string: String))) :: Nil) = c.prefix.tree

    val parsed = EnhancedStringFormatParser.parse(string)

    compiled(parsed, lit.pos)
  }
}
