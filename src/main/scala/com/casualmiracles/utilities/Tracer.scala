package com.casualmiracles.utilities

/**
 * @author yl
 */
class Tracer {
  var tracing: Boolean = true
  var mode: String = "output"
  private val accumulator = new StringBuilder() 
    
  def trace(i:Int, s:String ): Unit = if(tracing) { output("\t"*i); outputln(i, s) }
  
  def output(x:Any):Unit = {
    mode match{
      case "acc" => accumulator ++= x.toString
      case _ => print(x)
    }
  }
  
  def outputln(x: Any):Unit = output( x, "\n" )
  
  def clear(): Unit = accumulator.clear()
  
  def output(): String = accumulator.toString()
}
