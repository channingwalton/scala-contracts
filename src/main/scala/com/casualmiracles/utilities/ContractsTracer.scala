package com.casualmiracles.utilities

import org.casualmiracles.finance.contracts.Contract
import org.casualmiracles.finance.contracts.PR
import org.casualmiracles.finance.contracts.PR._

/**
 * @author yl
 */
class ContractsTracer extends Tracer{
  val tracehorizon: Int = 7
  
  def trace(i:Int, c:Contract):Unit = if(tracing) { output("\t"*i); outputln(c) }
  def trace[T](i:Int, s:String, pr: PR[T]): PR[T] = {
    if(tracing){ output("\t"*i); outputln(i,s); outputln( formatPr(pr, tracehorizon, "\t"*i ) ) }
    pr
  } 
}