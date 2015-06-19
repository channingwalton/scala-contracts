package com.casualmiracles.utilities

import org.casualmiracles.finance.contracts.Contract
import org.casualmiracles.finance.contracts.PR
import org.casualmiracles.finance.contracts.PR._

/**
 * @author yl
 */
class ContractsTracer extends Tracer{
  val tracehorizon: Int = 7
  
  def trace(i:Int, c:Contract):Unit = if(trace) { print("\t"*i); println(c) }
  def trace[T](i:Int, s:String, pr: PR[T]): PR[T] = {
    if(trace){ print("\t"*i); println(i,s); println( formatPr(pr, tracehorizon, "\t"*i ) ) }
    pr
  } 
}