package net.virtualvoid.string

import language.experimental.macros

case class WithIP(original: String) {
  def ip: String = macro EnhancedStringMacro.enhance

}

trait ESImplicits {
  implicit def strWithIP(string: String): WithIP = new WithIP(string)
}
