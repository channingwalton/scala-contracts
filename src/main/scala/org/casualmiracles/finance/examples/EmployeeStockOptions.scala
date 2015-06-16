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

  val sigma = 0.25
  val step = 1.0/5.0
  val U = EmployeeStockOptionsModel.up(sigma, step)
  val D = EmployeeStockOptionsModel.down(U)

  def europeanCallNoDividend(){
    val p = EmployeeStockOptionsModel.p(0.05, 0, step, U, D)
    
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), p )
    val esoevalX: Contract ⇒ PR[Double] = EmployeeStockOptionsModel.evalC(esoxm, USD)
      
    //val stock:PR[Double] = ratesUpDown(100.0, 1.1183, 0.8942)
    val cstock: Contract = stock(mkDate(0), 100, USD) 
    
    
    println( "Stock lattice:")
    println( formatPr(esoevalX(cstock), 10))
    
    // European call 
    println( "European Call:")
    val ec = europeancall(mkDate(5),cstock,100,USD)
    val ecpr = esoevalX(ec)
    println("--------------------")
  }
  
  def americanCallDividend(){
    val p = EmployeeStockOptionsModel.p(0.05, 0.04, step, U, D)
    
    // TODO: refactor code duplication by accessing p via function
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), p )
    val esoevalX: Contract ⇒ PR[Double] = EmployeeStockOptionsModel.evalC(esoxm, USD)
    
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