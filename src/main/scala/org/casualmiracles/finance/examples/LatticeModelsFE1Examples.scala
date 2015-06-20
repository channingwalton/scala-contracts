package org.casualmiracles.finance.examples

// Term Structure Lattice Models base on
// 2010 Martin Haugh
// http://www.columbia.edu/~mh2078/LatticeModelsFE1.pdf
 
import org.casualmiracles.finance.contracts._
import Contracts._
import Instruments._
import org.casualmiracles.finance.models._

object LatticeModelsFE1Examples extends App {

  val mps = new ModelParameters().p(0.5)
  
  val ltxm = LatticeFE1Model.makeModel(mkDate(0), mps )
  
  
  println("Default Interest Rates")
  println( formatPr(ltxm.rateModel( USD),6) )
  

  // TODO: How to initialize with different ratesModels[] map?
  //LatticeFE1Model.rateModels(USD) = LatticeFE1Model.ratesUpDown(6, 1.25, 0.9)
  
  
  
  
  val ltevalX=LatticeFE1Model.evalC(ltxm,USD)
  
  val ltc1:Contract = zeroCouponBond(mkDate(4),100,USD)
  val ltpr1 = ltevalX(ltc1)
  
  println("zcb:")
  println( formatPr(ltpr1,5) ) 

  // Buy contract
  val b1 = buy(ltc1, 84, USD)
  val b1pr = ltevalX(b1)
  println("---------------------")

  // European call 
  println( "European Call:")
  val ec = europeancall(mkDate(2),ltc1,84,USD)
  val ecpr = ltevalX(ec)
  println("--------------------")

  
  // American call 
  println( "American Put:")
  val ac = americanput(mkDate(3),ltc1,88,USD)
  val acpr = ltevalX(ac)
  println("--------------------")
  
  
  //xx val ec:Contract = european(mkDate(2), ltc1)
  //xx val ecpr = ltevalX(ec)
  //xx println("European Call:")
  //xx println( ec )
  //xx println( formatPr(ecpr, 3) )
  
  // American Option (+snell)
  
  
  // TODO: add the rest of the paper securities
 
  //assert( List( testK, testProb, testPr1).forall(identity), "Test suite falure")
}