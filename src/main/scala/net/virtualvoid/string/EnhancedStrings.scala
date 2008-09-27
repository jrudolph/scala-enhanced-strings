package net.virtualvoid.string

// The Visible API

trait IObjectFormatterFactory {
  def format(format:String,o:AnyRef):String
  def formatter[T<:AnyRef](format:String):IObjectFormatter[T]
}

trait IObjectFormatter[T<:AnyRef] {
  def format(o:T):String
}

object ObjectFormatter extends IObjectFormatterFactory{
  val parser = EnhancedStringFormatParser

  def formatter[T<:AnyRef](fm:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val parsed = parser.parse(fm)
    def format(o:T) = parsed.map(_.eval(o)) mkString ""
  }

  def format(format:String,o:AnyRef) = formatter(format).format(o)
}
