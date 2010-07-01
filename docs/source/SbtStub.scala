package sbt {
  trait ProjectInfo
  class DefaultProject(info: ProjectInfo)
  trait AutoCompilerPlugins {
    def compilerPlugin(str: String) = null
  }
}

package object sbt { 
  implicit def strHasPercentOps(str: String) = new {
    def %(next: String): String = next
    def %%(next: String): String = next
    def at(next: String): String = next
  }  
}

