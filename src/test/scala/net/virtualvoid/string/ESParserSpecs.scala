package net.virtualvoid.string

import _root_.org.specs._

object ParserSpecs extends Specification {
  import EnhancedStringFormatParser.{parse,lexical}
  import lexical._

  "The parser" should parseCorrectly {
    "'test'" in {"test" must beParsedAs(Literal("test"))}
    "'#prop'" in {"#prop" must beParsedAs(Exp("prop"))}
    "'#{prop}'" in {"#{prop}" must beParsedAs(Exp("prop"))}
    "'#{prop.member}'" in {"#{prop.member}" must beParsedAs(ParentExp(Exp("member"),"prop"))}
    "'#prop.member'" in {"#prop.member" must beParsedAs(ParentExp(Exp("member"),"prop"))}
    "'#listProp*'" in {"#listProp*" must beParsedAs(splice(Exp("listProp"),"",ThisExp))}
    "'#listProp{,}*'" in {"#listProp{,}*" must beParsedAs(splice(Exp("listProp"),",",ThisExp))}
    "'#{listProp}{,}*'" in {"#{listProp}{,}*" must beParsedAs(splice(Exp("listProp"),",",ThisExp))}
    "'#listProp[test]{,}*'" in {"#listProp[test]{,}*" must beParsedAs(splice(Exp("listProp"),",",Literal("test")))}

    "#this" in {"#this" must beParsedAs(ThisExp)}
    "#{this}" in {"#{this}" must beParsedAs(ThisExp)}
    "#this[]*" in {"#this[]*" must beParsedAs(splice(ThisExp,""))}

    //escaped square brackets
    "[[abc]]" in {"[[abc]]" must beParsedAs(Literal("[abc]"))}

    //escaped hash
    "##abc" in {"##abc" must beParsedAs(Literal("#abc"))}

    // more complex escape situations
    "##]] ####blub ##[[" in {"##]] ####blub ##[[" must beParsedAs(Literal("#] ##blub #["))}

    // test weird control combinations
    "Dots in normal literals 'This is a sentence.'" in {"This is a sentence." must beParsedAs(Literal("This is a sentence."))}
    "Dots after curly braced expressions 'This is a #{exp}.'" in {"This is a #{exp}." must beParsedAs(Literal("This is a "),Exp("exp"),Literal("."))}

    "Curly Braces somewhere in between 'This is {braced}'" in {"This is {braced}" must beParsedAs(Literal("This is {braced}"))}
    
    // conversions
    "date conversion" in {"#this->date[dd.MM.yyyy]" must beParsedAs(DateConversion(ThisExp,"dd.MM.yyyy"))}
    
    // conditionals
    "conditionals" in {"#this?[#this|Nope]" must beParsedAs(Conditional(ThisExp,List(ThisExp),List(Literal("Nope"))))}
  }

  // helper methods

  import org.specs.matcher.Matcher
  def beParsedAs(ts:StrToken*) = new Matcher[String]{
    val tokens = List(ts:_*)
    def apply(str: => String) = {
      val l = parse(str)
      (l.toString == tokens.toString,"equal",l.toString + " is not equal to the expected " + tokens.toString)
    }
  }
  def splice(e:Exp,sep:String,inner:StrToken*) = SpliceExp(e,sep,List(inner:_*))

  import org.specs.specification.Example
  def parseCorrectly(e: =>Example) = { currentSut.verb += " parse correctly"; e }
}

class ParserSpecsJUnit4 extends runner.JUnit4(ParserSpecs)