package net.virtualvoid.strings

object Stuff extends ScalaEnhancedStrings {
  val license = "BSD License"

  trait Facts {
    class EnhancedStringsPlugin {
      val runsAfter = "parser"
    }
    
    /* To see the code the plugin generates use this command line
       while compiling:

       scalac -Xplugin:... -Xprint:enhanced-strings */
  }

  trait KnownIssues {
    /*
       - The parser may a bit unreliable at the edges.
       - Missing specification for min/max-lengths for replacements like in */
         String.format /*
       - More conversions needed       
     */
  }
  trait Contact {
    val authorsEmail = "johannes.rudolph@gmail.com"
    val mailingList = "http://groups.google.com/group/scala-enhanced-strings"
    val bugTracker = "http://github.com/jrudolph/scala-enhanced-strings/issues"
  }
}

/* SeS - Scala Enhanced Strings
 * Copyright 2008 - 2010 Johannes Rudolph
 * Visit http://github.com/jrudolph/scala-enhanced-strings
 * File bugs at http://github.com/jrudolph/scala-enhanced-strings/issues
 * Mailing list at http://groups.google.com/group/scala-enhanced-strings
 */
