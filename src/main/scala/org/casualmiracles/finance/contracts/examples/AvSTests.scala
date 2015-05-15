package org.casualmiracles.finance.contracts.examples

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.contracts.examples._
import Contracts._
import ExampleModel._
import Instruments._

// Appendix A Tests from Anton Van Straaten's doco webpage
object AvSTests extends App{

  
  val tolerance = 0.001
  
  val testK = andPr( liftPr((d:Double)=>d==100, takePr(10, bigK[Double](100))))
 
  val testProb = probabilityLattice(10).sum - 1 < tolerance
  
  val xm = exampleModel(mkDate(0))
  val evalX=evalC(xm,USD)

  val c1:Contract = zeroCouponBond(mkDate(3),10,USD)
  val pr1 = evalX(c1)

  val testPr1 = andPr(
   lift2Pr( (a:Double, b:Double) => math.abs(a - b) < tolerance, pr1, PR( Stream[RV[Double]]( 
      Stream[Double](8.641), 
      Stream[Double](9.246,8.901),
      Stream[Double](9.709,9.524,9.346), 
      Stream[Double](10,10,10,10)
    ) )
   )
  )
 
  assert( List( testK, testProb, testPr1).forall(identity), "Test suite falure")
}