import sbt._

class ESUsageExample(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  val es = compilerPlugin("net.virtualvoid" %% "scala-enhanced-strings" % "1.0")
}
