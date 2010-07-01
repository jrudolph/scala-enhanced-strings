package net.virtualvoid.strings

object Installation extends ScalaEnhancedStrings 
  with StandaloneUsage
  with UseWithSBT 

trait StandaloneUsage {
  /*
     - Grab the latest jar file from http://github.com/jrudolph/scala-enhanced-strings/downloads.
     - Call scalac with -Xplugin:<path-to-jar>
     - That's it.
   */
}

trait UseWithSBT {
  /* See the usage example at http://github.com/jrudolph/scala-enhanced-strings/tree/master/usage
     or here: */
  
  /* Starting with a normal project descriptor... */
  import sbt._

  class ESUsageExample(info: ProjectInfo) 
    extends DefaultProject(info) 
    /* ... extend your project descriptor */
    with AutoCompilerPlugins {
  
    val virtualVoid = "Virtual-Void repository" at "http://mvn.virtual-void.net"

    val es = compilerPlugin("net.virtualvoid" %% "scala-enhanced-strings" % "0.5")
  }
}

object Usage extends ScalaEnhancedStrings {
  /* Make sure you have installed the jar as described in */ Installation /*
     At the scope where you want to use enhanced strings put a marker annotation:    */

  @EnhanceStrings // enhance strings in this scope
  trait Example1 {
    val x = 5
    val str = "Inner string arithmetics: #{{ x * x + 12 }}"
  }

  /* The current enhanced string syntax is not fixed. Actually, it is
     just a particular flavor. In the future there may be other
     flavors. You can specify which version you programmed against by
     specifying it in the @EnhancedStrings annotation. So your program
     is compatible even with */ new ScalaEnhancedStrings /* versions */

  @EnhanceStrings(syntax= "poros", version = 1) // poros is the default syntax flavor
  trait Example2 {
    val x = "The class of my parent object is: #Usage"
  }

  /* Next chapter: */ Syntax
}

