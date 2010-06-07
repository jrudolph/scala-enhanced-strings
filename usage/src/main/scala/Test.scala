object Test {
  @EnhancedString(syntax="poros")
  def main(args: Array[String]) {
    println("These are the arguments: #args{, }*")
  }
}
