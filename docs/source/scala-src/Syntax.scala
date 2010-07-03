package net.virtualvoid.strings

@EnhanceStrings(syntax="poros", version=1)
object Syntax extends ScalaEnhancedStrings {
  /* The plugin reinforces string constants with several new possibilities
     to add data into a String.

     The easiest enhanced string is just a literal string without any
     control characters. */
  val literalString = "This is just a normal String"

  /* For the more interesting features, the two important concepts to
     know are */ Expressions; /* and */ Conversions

  object Expressions {

    /* Expressions are introduced by the # (sharp) sign */
    val x = 5
    val stringWithNumber = "The number is #x!"

    /* With this syntax, you can access the value of a variable from
       the scope. Properties (ie methods with no parameters)
       can be accessed with the usual dot notation. */
    val aString = "test"
    val info = "aString has the length #aString.length"

    /* To declare unambigiously where an expression ends, put it into
       braces */
    val braced = "Length: #{aString.length}.stuff"

    // val unbraced = "Length: #aString.length.stuff"
    // error here: "value stuff is not a member of Int"

    /* Up until now, you've just seen simple expressions, the full power
       unleashes when you insert arbitrary Scala expressions into a string
       using #{{ ... }} */ 
    val inStringCalculation = "What is x + 5 + 12? #{{ x + 5 + 12 }}"
  }
  
  /* Not all expressions evaluate to a String value. There are some
     convenience */ Conversions /* built directly into the syntax */
  
  object Conversions {
    /* For each expression, a conversion to String is needed. If you
       don't define any conversion, the value is converted by calling
       the `toString` method of the value.

       Recall the example from the last section: */

    val aString = "test"
    val info = "aString has the length #aString.length"
    
    aString.length /* is of type Int, so it has to be converted to a String
       before it can be appended to the rest of the String. Behind the scenes,
       ScalaEnhancedStrings creates code like this: */

    val infoAfterSeS = "aString has the length "+aString.length.toString


    /* More interesting is the set of built-in conversions and the
       syntax to use them. (Note: the set of conversions is by no means
       complete yet.  If you are missing something, please say so.)

       First, there is support for conditional string formatting with the
       conversion syntax `?[<then-string>|<else-string>]` */

    val isSummer = true
    def proclaimTemperature =
      println("It is so #isSummer?[hot|cold]")

    /* For */ Option/*-al values the same syntax applies. In the
     <then-string> there is the expression #it or #this available,
     which refers to the contents of the */ Option /* value */

    val optionalBeer: Option[String] = Some("Rothaus")
    val thirstyMan = "He's drinking #optionalBeer?[his #it|nothing]"
    // evaluates to "He's drinking his Rothaus"

    /* Then, there's support for formatting whole sequences of values (of type Iterator)
       by using the `[<inner_formatter>]{<separator>}*` syntax.
       Inside of the <inner_formatter> you can access the current element
       by #it or #this. The asterisk '*' at the end in enhanced string syntax
       instructs the plugin to expand all these values, format each element
       as specified with the inner formater and then join them together
       using the separator */

    val fruit = List("Apple", "Orange", "Lemon", "Banana")
    val breakfast = "Today there's #fruit[the tasty #it]{, }* in my yoghurt"
    // == "Today there's the tasty Apple, the tasty Orange ...

    /* The inner part of the last string could be rewritten as */
    fruit.map(it => "the tasty "+it.toString).mkString(", ")
    /* in fact, this is exactly what the plugin does. */
  
    /* You can omit the <inner_formater> in which case #it is assumed. You
       can omit the separator, as well, in which case the separator is the empty string. */
  
    val allDigits = "#{{0 to 9}}*" // == "0123456789"

  }

  /* next chapter: */ Stuff
}

/* SeS - Scala Enhanced Strings
 * Copyright 2008 - 2010 Johannes Rudolph
 * Visit http://github.com/jrudolph/scala-enhanced-strings
 * File bugs at http://github.com/jrudolph/scala-enhanced-strings/issues
 * Mailing list at http://groups.google.com/group/scala-enhanced-strings
 */
