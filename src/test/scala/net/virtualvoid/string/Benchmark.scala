package net.virtualvoid.string

object Benchmark {
  class Test{
    val string = "This is a Test"
    val chars = string.toCharArray.map(Character.valueOf(_))
    
    def characters = chars
    def option = Some("test")
  }
  
  val formats = List(
    ("simple literal","abcdefghijklmnopqrstuvwxyz"),
    ("method access","String characters: #characters"),
    ("array expansion","Characters: #characters[#this]{,}*"),
    ("option array","Option: #option?[Yes: #this|No]")
  )
  
  val sampleObject = new Test
  
  val formatFactories = List(ObjectFormatter,FormatCompiler)
  
  def benchmark(times:Int,formatter:IObjectFormatter[Test]) = {
	val startTime = System.nanoTime
	
	var i = times
	do{
	  formatter.format(sampleObject)
	  i = i - 1;
	}while(i>0)
   
	System.nanoTime - startTime
  }
  
  def main(args:Array[String]){
	val times = args(0).toInt
	
	for((formatName,format)  <- formats;
	  	factory <- formatFactories;
		formatter = factory.formatter(classOf[Test],format)){
	  System.out.println(factory.getClass.getSimpleName+":"+formatName+" "+(benchmark(times,formatter)/1e6)+" ms")
	}		
  }
}
