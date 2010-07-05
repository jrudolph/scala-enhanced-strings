package net.virtualvoid.strings

object Stuff extends ScalaEnhancedStrings {
  val license = "BSD License"

  trait Facts {
    class EnhancedStringsPlugin {
      val runsAfter = "parser"
    }
    
    /*
       - To see the code the plugin generates use this command line
	 while compiling:
         
	 scalac -Xplugin:... -Xprint:enhanced-strings
         
       - Most of the plugin is a spin-off of work I've done
         before. When the topic of variable interpolation was
         mentioned on the mailing list in summer 2009 I could quickly
         hack together a basically working version because I've
         already had a parser available. Literal Scala expressions
         aside, you can use the formatter without the plugin
         standalone, doing all the parsing at runtime. There's a
         currently unmaintained module which compiles a formatting
         string to bytecodes at runtime, as well.  */
  }
  trait IDEIntegration {
    /* Syntax highlighting doesn't work in IDEs. However, at least in ensime
       the common features are working: error reporting, showing types, inspecting
       variables. (Only, if you make ensime compile with the plugin enabled, which
       I managed to do only by enabling it directly in ensime's sources.)
     */
  }
  trait KnownIssues {
    /*
       - The parser may a bit unreliable at the edges.
       - More conversions needed       
     */
  }
  trait Contact {
    val authorsEmail = "johannes.rudolph@gmail.com"
    val mailingList = "http://groups.google.com/group/scala-enhanced-strings"
    val bugTracker = "http://github.com/jrudolph/scala-enhanced-strings/issues"
  }

  /* This documentation was formatted by Mark Harrah's sxr */
}

/* SeS - Scala Enhanced Strings
 * Copyright 2008 - 2010 Johannes Rudolph
 * Visit http://github.com/jrudolph/scala-enhanced-strings
 * File bugs at http://github.com/jrudolph/scala-enhanced-strings/issues
 * Mailing list at http://groups.google.com/group/scala-enhanced-strings
 */
