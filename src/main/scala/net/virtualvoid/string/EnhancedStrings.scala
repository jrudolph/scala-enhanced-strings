package net.virtualvoid.string

// The Visible API

trait IObjectFormatterFactory {
  def format(format:String,o:AnyRef):String = formatter(format).format(o)
  def formatter[T<:AnyRef](format:String):IObjectFormatter[T]
}

trait IObjectFormatter[T<:AnyRef] {
  def format(o:T):String
}