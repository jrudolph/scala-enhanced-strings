object Test {
  @EnhancedString(syntax="0.2")
  def main(args: Array[String]) {
    println("These are the arguments: #args{,}*")
  }
}
