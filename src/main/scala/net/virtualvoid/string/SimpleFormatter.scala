/* SeS - Scala Enhanced Strings
* Copyright 2008 - 2010 Johannes Rudolph
*/

package net.virtualvoid.string

object ObjectFormatter extends IObjectFormatterFactory{
  val parser = EnhancedStringFormatParser

  def formatter[T<:AnyRef](clazz:Class[T],fm:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val parsed = parser.parse(fm)
    def format(o:T) = parsed.format(o)
  }
}
