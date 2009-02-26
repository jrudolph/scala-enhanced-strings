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
        System.out.println("run")
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
 
	val averageOf = 20
	
	for((formatName,format)  <- formats;
	  	factory <- formatFactories)
	  if (withCompileTimes) 
		  System.out.println(factory.getClass.getSimpleName+":"+formatName+" "+(benchmarkWithCompilation(times,factory,format)/1e6)+" ms")
	  else {
		  val formatter = factory.formatter(classOf[Test],format)

		  val name = factory.getClass.getSimpleName+":"+formatName
		  
		  System.out.println(" + Warming up for "+name)
		  if (warmup)
			  benchmark(times*10,formatter)
		  System.out.println(" - Warming up finished for "+name)
		  
		  System.out.println(" + Running "+name)

	      val results:Array[Long] = (0 until averageOf).map(i => benchmark(times,formatter)).toArray
		  System.out.println(" - Finished running "+name)
		  //for(res <- results) System.out.println(res/1e6)
       
		  val avg = results.foldLeft(0.)(_+_.toDouble)/averageOf
		  val variance = results.foldLeft(0d)((sum,x)=> sum + (x-avg)*(x-avg))/(averageOf-1.)
		  val stdev = Math.pow(variance,.5)
		  
		  def ind(x:long) = (x-avg) match{
		    case s if s <= -2*stdev => '↡'
		    case s if s <= -stdev   => '↓'
		    case s if s <= -stdev/2.=> '↘'
		    case s if s <= stdev/2  => '→'
		    case s if s <= stdev    => '↗'
		    case s if s <= 2*stdev  => '↑'
		    case _	                => '↟'
		  }
    
		  val indicators:CharSequence = results.map(ind(_))
		  
	      System.out.println(name+" Average of "+averageOf+" runs: "+avg/1e6+" ms +/- "+stdev/1e6+" ms = "+stdev*100./avg+"%"+"\n["+indicators+"]")
	  }
  }
}
