package org.casualmiracles.finance.models

import org.scalatest._
import Matchers._

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.models._
import EberModel._
import Contracts._
import Instruments._
import org.casualmiracles.utilities.ContractTestUtils

import scala.Stream

/**
 * @author yl
 */
// Appendix A Tests from Anton Van Straaten's doco webpage
class AvSSuite extends FunSuite with Matchers with ContractTestUtils{

  test("Constant Lattice and Basic Lattice Operations"){
    andPr( liftPr((d:Double)=>d==100, takePr(10, bigK[Double](100)))) should be (true) 
  }
  
  test("Probability Lattice"){
    probabilityLattice(10).sum -1 should be < tolerance    
  }
  
  test("Zero Coupon Bond and Expected Value"){

    val mps = new ModelParameters().p(0.5)
    
    mps.toString() should be ("sigma: 0.0; r: 0.0; div: 0.0; step: 0.0; up: 1.0; down: 1.0; p: 0.5")
    
    val xm = EberModel.makeModel(mkDate(0), mps)
    val evalX=evalC(xm,USD)
  
    val c1:Contract = zeroCouponBond(mkDate(3),10,USD)
    val pr1 = evalX(c1)

    compare2Pr( pr1, PR( Stream[RV[Double]]( 
        Stream[Double](8.642), 
        Stream[Double](9.246,8.901),
        Stream[Double](9.709,9.524,9.346), 
        Stream[Double](10.0,10.0,10.0,10.0)
      ) ), 0.001 
      ) should be (true) 
  
    
    val ev1 = expectedValuePr( pr1 )
    (ev1, Stream[Double](8.642, 9.0736, 9.526, 10.0) ).zipped.
        map({case(a,b)=>Math.abs(a- b)<tolerance}).
        forall(identity) should be (true)  
  }
  
}