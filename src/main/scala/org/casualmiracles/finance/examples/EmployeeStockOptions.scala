package org.casualmiracles.finance.examples

// After The binomial lattice option-pricing model for valuing Americantype
// employee stock options
// By Anthony R. Banks*
//
// Part 1: http://www.bvresources.com/pdfs/WB081513/BVU%200805.pdf
// Part 2: http://www.bvresources.com/pdfs/WB081513/BVU%200905.pdf

import org.casualmiracles.finance.contracts._
import Contracts._
import Instruments._
import org.casualmiracles.finance.models._


/**
 * @author yl
 */
object EmployeeStockOptions extends App with GeometricInterestRateModel{

  def europeanCallNoDividend(){

    val mps = new ModelParameters( 0.05, 0.25, 0, 1.0/5.0 )

//    // TODO: uncomment after convertion to unit test
//    mps.toString() should be (
//      "sigma: 0.25; r: 0.05; div: 0.0; step: 0.2; up: 1.118292981373268; down: 0.8942200448866237; p: 0.5169304424430043"    
//    )
    
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), mps)
   
    val esoevalM = new EmployeeStockOptionsModel()

    esoevalM.tracer.tracing = true
    
    val esoevalX = esoevalM.evalC(esoxm, USD)
    
      
    //val stock:PR[Double] = ratesUpDown(100.0, 1.1183, 0.8942)
    val cstock: Contract = stock(mkDate(0), 100, USD) 
    
    
    println( "Stock lattice:")
    println( formatPr(esoevalX(cstock), 10))
    esoevalM.tracer.tracing = true
    
    // European call 
    println( "European Call:")
    val ec = europeancall(mkDate(5),cstock,100,USD)
    val ecpr = esoevalX(ec)
    println("--------------------")
  }
  
  def americanCallDividend(){
    
    val mps = new ModelParameters( 0.05, 0.25, 0.04, 1.0/5.0 )
    
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), mps )
    
    val esoevalM = new EmployeeStockOptionsModel()
    
    val esoevalX: Contract ⇒ PR[Double] = esoevalM.evalC(esoxm, USD)
    
    val cstock: Contract = stock(mkDate(0), 100, USD) 
    // American call 
    println( "American Call:")
    val ac = americancall(mkDate(5),cstock,100,USD)
    val acpr = esoevalX(ac)
    println("--------------------")
  }
  europeanCallNoDividend
  americanCallDividend
}