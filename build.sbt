organization := "net.virtual-void"

name := "scala-enhanced-strings"

version := "0.6.0-SNAPSHOT"

homepage := Some(url("http://jrudolph.github.com/scala-enhanced-strings"))

licenses in GlobalScope += "BSD" -> url("https://raw.github.com/jrudolph/scala-enhanced-strings/master/LICENSE")

libraryDependencies += "org.scala-tools.testing" % "specs_2.9.0-1" % "1.6.8" % "test"

libraryDependencies <+= scalaVersion ("org.scala-lang" % "scala-compiler" % _)

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions in (Compile, console) <+= (packageBin in Compile) map { bin =>
  "-Xplugin:"+bin.absolutePath
}

//seq(lsSettings :_*)

//(LsKeys.tags in LsKeys.lsync) := Seq("compiler-plugin", "string", "syntax", "string-interpolation")

//(LsKeys.docsUrl in LsKeys.lsync) <<= homepage

//(externalResolvers in LsKeys.lsync) := Seq(
//  "Virtual-Void repository" at "http://mvn.virtual-void.net")

//(description in LsKeys.lsync) :=
//  "A scalac compiler plugin that adds string interpolation and more to scala."

resolvers += "snaps" at "https://oss.sonatype.org/content/repositories/snapshots/"

scalaVersion := "2.10.0-SNAPSHOT"

