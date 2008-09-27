package net.virtualvoid.string

object ObjectFormatter extends IObjectFormatterFactory{
  val parser = EnhancedStringFormatParser

  def formatter[T<:AnyRef](fm:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val parsed = parser.parse(fm)
    def format(o:T) = parsed.map(_.eval(o)) mkString ""
  }
}
