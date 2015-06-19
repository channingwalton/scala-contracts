package org.casualmiracles.utilities

import org.scalatest.FunSuite
import org.scalatest.Matchers
import com.casualmiracles.utilities.Tracer

/**
 * @author yl
 */
class TracerSuite extends FunSuite with Matchers{
  
  test("Tracer accumulator"){
    
    val tracer = new Tracer()
    
    // Default: tracing, console mode
    tracer.trace(5, "String")
    
    tracer.output() should be ( "" )

  
    // tracing, accumulator mode
    tracer.mode = "acc"
    tracer.trace(0, "String")
    
    tracer.output() should be ("((0,String),\n)")
    

    // no tracing 
    tracer.clear()
    tracer.trace = false
    
    tracer.trace(0, "String")
    
    tracer.output() should be ("")
  }
}