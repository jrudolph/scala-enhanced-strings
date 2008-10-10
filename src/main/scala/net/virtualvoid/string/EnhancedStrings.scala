package net.virtualvoid.string

// The Visible API

trait IObjectFormatterFactory {
  def format[T<:AnyRef](format:String,o:T):String = formatter(o.getClass.asInstanceOf[Class[T]],format).format(o)
  def formatter[T<:AnyRef](clazz:Class[T],format:String):IObjectFormatter[T]
}

trait IObjectFormatter[T<:AnyRef] {
  def format(o:T):String
}