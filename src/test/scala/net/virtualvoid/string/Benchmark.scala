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
  
  def benchmarkWithCompilation(times:Int,factory:IObjectFormatterFactory,format:String) = {
	val startTime = System.nanoTime
 
	val formatter = factory.formatter(classOf[Test],format)
	
	var i = times
	do{
	  formatter.format(sampleObject)
	  i = i - 1;
	}while(i>0)
   
	System.nanoTime - startTime
  }
  
  def main(args:Array[String]){
	val times = args(0).toInt
	val warmup = args(1) == "w"
	val withCompileTimes = args(1) == "c"
	
	for((formatName,format)  <- formats;
	  	factory <- formatFactories)
	  if (withCompileTimes) 
		  System.out.println(factory.getClass.getSimpleName+":"+formatName+" "+(benchmarkWithCompilation(times,factory,format)/1e6)+" ms")
	  else {
		  val formatter = factory.formatter(classOf[Test],format)

		  if (warmup)
			  benchmark(times*10,formatter)
		  
		  System.out.println(factory.getClass.getSimpleName+":"+formatName+" "+(benchmark(times,formatter)/1e6)+" ms")
	  }
  }
}
