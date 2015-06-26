package org.casualmiracles.finance.models

import org.scalatest.FunSuite
import org.scalatest.Matchers

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.contracts.Contracts._
import org.casualmiracles.finance.contracts.Instruments._
import org.casualmiracles.finance.models._
import EmployeeStockOptionsModel._
import org.casualmiracles.utilities.ContractTestUtils

// After The binomial lattice option-pricing model for valuing American type of
// employee stock options
// By Anthony R. Banks*
//
// Part 1: http://www.bvresources.com/pdfs/WB081513/BVU%200805.pdf
// Part 2: http://www.bvresources.com/pdfs/WB081513/BVU%200905.pdf

/**
 * @author yl
 */
class EmployeeStockOptionsSuite extends FunSuite with Matchers with ContractTestUtils{
  
  test( "European Call No Dividend" ){

    val mps = new ModelParameters( 0.05, 0.25, 0.00, 1.0/5.0 )

    mps.toString() should be (
      "sigma: 0.25; r: 0.05; div: 0.0; step: 0.2; up: 1.118292981373268; down: 0.8942200448866237; p: 0.5169304424430043"    
    )
    
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), mps)
   
    val esoevalM = new EmployeeStockOptionsModel()
 
    // Interest rate model lattice - constant 5 
    andPr( liftPr((d:Double)=>d==5.0, takePr(5,esoevalM.rateModels(USD))) ) should be (true)
    
    val esoevalX = esoevalM.evalC(esoxm, USD)

    // TODO: re-instate eventually dates and currency; was: stock(mkDate(0),100,USD)
    val cstock: Contract = stock( 100 ) 
    

   // Stock lattice
   val stockpr = esoevalX(cstock)

   val testStock = PR( Stream[RV[Double]]( 
        Stream[Double](100.00), 
        Stream[Double](89.42,111.83),
        Stream[Double](79.96,100.00,125.06), 
        Stream[Double](71.50,89.42,111.83,139.85),
        Stream[Double](63.94,79.96,100.00,125.06,156.39), 
        Stream[Double](57.18,71.50,89.42,111.83,139.85,174.90)
   ) ) 
    
    compare2Pr( stockpr, testStock ) should be (true)
      
    // European call 
    val ec = europeancall(mkDate(5),cstock, 100,USD)
    val ecpr = esoevalX(ec)
    
     val testOption = PR( Stream[RV[Double]]( 
          Stream[Double](12.79),
          Stream[Double](5.77,19.61),
          Stream[Double](1.59,9.79,29.17),
          Stream[Double](0.00,3.10,16.23,41.83),
          Stream[Double](0.00,0.00,6.05,26.05,57.39),
          Stream[Double](0.00,0.00,0.00,11.83,39.85,74.90)
     ) ) 
    compare2Pr( ecpr, testOption ) should be (true)
   }
  
  test( "American Call Dividend" ){
    
    // 29, 30, 40, 0, 30, 5, 0; Call American, 6 = 1.875
    val mps = new ModelParameters( 0.05, 0.25, 0.04, 1.0/5.0 )
    
    mps.toString() should be (
      "sigma: 0.25; r: 0.05; div: 0.04; step: 0.2; up: 1.118292981373268; down: 0.8942200448866237; p: 0.48101282616877233"    
    )
    
    val esoxm = EmployeeStockOptionsModel.makeModel(mkDate(0), mps )
    
    val esoevalM = new EmployeeStockOptionsModel()
         
    val esoevalX: Contract ⇒ PR[Double] = esoevalM.evalC(esoxm, USD)
    
    val cstock: Contract = stock(100) 
    
    // American call 
    val ac = americancall(mkDate(5),cstock,100,USD)
    val acpr = esoevalX(ac)
 
    
   val testOption = PR( Stream[RV[Double]]( 
        Stream[Double](10.50), 
        Stream[Double](4.68,17.00),
        Stream[Double](1.28,8.44,26.60),
        Stream[Double](0.00,2.68,14.83,39.85),
        Stream[Double](0.00,0.00,5.63,25.06,56.39),
        Stream[Double](0.00,0.00,0.00,11.83,39.85,74.90)
   ) ) 
   compare2Pr( acpr, testOption ) should be (true)
  }
}