package org.casualmiracles.finance.examples

// Term Structure Lattice Models base on
// 2010 Martin Haugh
// http://www.columbia.edu/~mh2078/LatticeModelsFE1.pdf
 
import org.casualmiracles.finance.contracts._
import Contracts._
import Instruments._
import org.casualmiracles.finance.models._
import LatticeFE1Model._

object LatticeModelsFE1Examples extends App {

  val ltxm = LatticeFE1Model.makeModel(mkDate(0) )
  printPr(ltxm.rateModel( USD),6) 
  
  val ltevalX=LatticeFE1Model.evalC(ltxm,USD)
  
  val ltc1:Contract = zeroCouponBond(mkDate(4),100,USD)
  val ltpr1 = ltevalX(ltc1)
  
  printPr(ltpr1,5) 
    
  // TODO: add the rest of the paper securities
 
  //assert( List( testK, testProb, testPr1).forall(identity), "Test suite falure")
}