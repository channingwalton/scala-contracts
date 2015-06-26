package org.casualmiracles.finance.models

import org.scalatest.FunSuite
import org.scalatest.Matchers

class ValuatorSuite extends FunSuite with Matchers{

  test("Eso Model for American Call on Stock/Hoadley") {
 
    val input = ContractParameters(
      "eso", "americancallonstock", 29.0, 30.0, 0.05, 0.30, 0.0, 40.0/365.0, 6,
      false, false)
    
    
    val output = Valuator.valuateContract(input)
    
    println(output)
    
    
    output.pr.get.unPr(0)(0) should be ( 1.8751879170094086)
  }
  
  test("Eso Model for American Call on Stock/Antony Banks") {
    val input = ContractParameters(
        "eso", "americancallonstock", 100.0, 100.0, 0.05, 0.25, 0.04, 1, 5, 
        true, false)
        
    val output = Valuator.valuateContract(input)
    
    output.pr.get.unPr(0)(0) should be ( 10.499751521439398)
  }
}