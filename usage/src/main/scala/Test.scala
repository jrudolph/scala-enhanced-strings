@EnhanceStrings(syntax="poros")
object Test {
  def main(args: Array[String]) {
    println("These are the arguments: #args{, }* {{ args(5) }}")
  }
}
