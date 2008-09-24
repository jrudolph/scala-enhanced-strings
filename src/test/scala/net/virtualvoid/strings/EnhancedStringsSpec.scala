package net.virtualvoid.string

import org.specs._

object ParserSpecs extends Specification {

  import EnhancedStringFormatParser.{parse,lexical}
  import lexical._

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
  }
}

/* Code awaiting testing
trait IAccount{
  def getBank():String
  def getAccountNo():String
  def getOwner():IPerson
}
trait IPerson{
  def getFirstName():String
  def getLastName():String
  def getAccounts():java.util.List[IAccount]
}
case class Account(nummer:String,bank:String,owner:IPerson) extends IAccount{
  def getBank:String = bank
  def getAccountNo:String = nummer
  def getOwner:IPerson = owner
}
object Peter extends IPerson{
  def getFirstName:String = "Peter"
  def getLastName:String = "Paul"
  def getAccounts():java.util.List[IAccount] =
    java.util.Arrays.asList(Array(Account("234234","Rich Bank Berlin",this),Account("3424234","Park Bank",this)))
}
System.out.println(eval("Name: #firstName #lastName\nAccounts:\n#accounts[#accountNo at #bank](\n)*",Peter))
System.out.println(eval("#this[#this has the length #length and consists of the chars #toCharArray['#this'](,)*](\n)*",Array("test","long string")))
*/