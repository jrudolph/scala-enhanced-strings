import sbt._

class ESUsageExample(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  val virtualVoid = "Virtual-Void repository" at "http://mvn.virtual-void.net"

  val es = compilerPlugin("net.virtualvoid" %% "scala-enhanced-strings" % "0.5.2")
}
