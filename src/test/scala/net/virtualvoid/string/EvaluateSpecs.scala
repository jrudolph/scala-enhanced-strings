package net.virtualvoid.string

import _root_.org.specs._

object EvaluateSpecs extends Specification{

  import java.lang.{String=>jString}
  case class Bank(n:String){
    def name():jString = n
  }
  case class Account(n:String,b:Bank) {
    def number():jString = n
    def bank() = b
  }
  class Person{
      def name():java.lang.String = "Joe"
      def accountNames():java.util.List[java.lang.String] = java.util.Arrays.asList("a","b")
      val sparkasse = Bank("Sparkasse")
      def accounts():java.util.List[Account] = java.util.Arrays.asList(Account("78910",sparkasse),Account("12345",Bank("Volksbank")))
      def accs():Array[Account] = accounts().toArray(new Array[Account](0))
  }
  val thePerson = new Person

  def evaluate(factory:IObjectFormatterFactory){
    import matcher.Matcher
    def evaluateAs(str:String) = new Matcher[String]{
      def apply(format: =>String) =
        (factory.format(format,thePerson) == str,"evaluates as "+str,"does not evaluate as "+str)
    }

    "literal" in {"literal" must evaluateAs("literal")}
    "property access" in {"#name" must evaluateAs(thePerson.name)}
    "string array access" in {"#accountNames*" must evaluateAs("ab")}
    "string array access with separator" in {"#accountNames{,}*" must evaluateAs("a,b")}
    "object iterable access with inner expression" in {"#accounts[#number]{,}*" must evaluateAs("78910,12345")}
    "object array access with inner expression" in {"#accs[#number]{,}*" must evaluateAs("78910,12345")}
    "deep property access" in {"#accs[#bank.name]{,}*" must evaluateAs("Sparkasse,Volksbank")}
  }

  "The format interpreter" should {
    evaluate(ObjectFormatter)
  }
  "The format compiler" should {
    evaluate(ObjectFormatter)
  }
}

class EvaluateSpecsTest extends runner.JUnit4(EvaluateSpecs)