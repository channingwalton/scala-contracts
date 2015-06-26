package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.contracts.Contracts._
import org.casualmiracles.finance.contracts.Instruments._
import org.casualmiracles.finance.models._
import LatticeFE1Model._
import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.casualmiracles.utilities.ContractTestUtils


// Term Structure Lattice Models base on
// 2010 Martin Haugh
// http://www.columbia.edu/~mh2078/LatticeModelsFE1.pdf

class LatticeModelsFE1Suite extends FunSuite with Matchers with ContractTestUtils {

  test("European Call and American Put on Zero-Coupon Bond"){
      
    val mps = new ModelParameters().up(1.25).down(0.9).p(0.5)
    
    // Check model parameters
    mps.toString() should be (
      "sigma: 0.0; r: 0.0; div: 0.0; step: 0.0; up: 1.25; down: 0.9; p: 0.5"    
    )        
    
    // Initialize with different ratesModels[] map?
    LatticeFE1Model.rateModels(USD) = LatticeFE1Model.ratesUpDown(6, 1.25, 0.9)  
    
    val testRateModel = PR( Stream[RV[Double]]( 
        Stream[Double](6.0), 
        Stream[Double](5.4,7.5), 
        Stream[Double](4.9,6.8,9.4), 
        Stream[Double](4.4,6.1,8.4,11.7), 
        Stream[Double](3.9,5.5,7.6,10.5,14.6), 
        Stream[Double](3.5,4.9,6.8,9.5,13.2,18.3)
    ) )   
    compare2Pr(LatticeFE1Model.rateModels(USD), testRateModel, 0.1 ) should be (true)

     
    val ltxm = LatticeFE1Model.makeModel(mkDate(0), mps )
    
    val ltevalX=LatticeFE1Model.evalC(ltxm,USD)
    
    val ltc1:Contract = zeroCouponBond(mkDate(4),100,USD)
    val ltpr1 = ltevalX(ltc1)
    
    
    // Check zero-coupon Bond lattice
    val testZcb = PR( Stream[RV[Double]]( 
        Stream[Double](77.22), 
        Stream[Double](84.43,79.27), 
        Stream[Double](90.64,87.35,83.08), 
        Stream[Double](95.81,94.27,92.22,89.51), 
        Stream[Double](100.00,100.00,100.00,100.00,100.00)
    ) )
    compare2Pr( ltpr1, testZcb ) should be (true)
  
    // Buy contract
    val b1 = buy(ltc1, 84, USD)
    val b1pr = ltevalX(b1)
      
    val testBuyContract = PR( Stream[RV[Double]]( 
      Stream[Double](-6.782),
      Stream[Double](0.434,-4.732),
      Stream[Double](6.636,3.350,-0.924),
      Stream[Double](11.809,10.273,8.219,5.510),
      Stream[Double](16.000,16.000,16.000,16.000,16.000)
    ) )        
    compare2Pr( b1pr, testBuyContract, 0.1 ) should be (true)
  
    // European Call 
    val ec = europeancall(mkDate(2),ltc1,84,USD)
    val ecpr = ltevalX(ec)

    val testEuropeanCall = PR( Stream[RV[Double]]( 
      Stream[Double](2.97),
      Stream[Double](4.74,1.56),
      Stream[Double](6.64,3.35,0.00)
    ) )     
    compare2Pr(ecpr,testEuropeanCall) should be (true) 
    
    // American Put 
    println( "American Put:")
    val ac = americanput(mkDate(3),ltc1,88,USD)
    val acpr = ltevalX(ac)

    
    val testAmericanPut = PR( Stream[RV[Double]]( 
      Stream[Double](10.78),
      Stream[Double](3.57,8.73),
      Stream[Double](0.00,0.65,4.92),
      Stream[Double](0.00,0.00,0.00,0.00)
    ) )     
    compare2Pr(acpr,testAmericanPut) should be (true) 
   
  }
  // TODO: add the rest of the paper securities

}