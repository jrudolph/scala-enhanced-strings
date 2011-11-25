organization := "net.virtualvoid"

name := "scala-enhanced-strings"

version := "0.5.2"

libraryDependencies ++=
  Seq("org.scala-tools.testing" % "specs_2.9.0-1" % "1.6.8" % "test",
      "org.scala-lang" % "scala-compiler" % "2.9.1")

scalacOptions += "-unchecked"

(packageBin in Compile) <<= (packageBin in Compile).dependsOn(test in Test)
