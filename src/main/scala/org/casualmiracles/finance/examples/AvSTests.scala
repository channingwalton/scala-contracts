package org.casualmiracles.finance.examples

import org.casualmiracles.finance.contracts._
import org.casualmiracles.finance.models._
import org.casualmiracles.finance.contracts.Contracts._
import ExampleModel._
import org.casualmiracles.finance.contracts.Instruments._
import org.casualmiracles.finance.models._
import scala.Stream

// Appendix A Tests from Anton Van Straaten's doco webpage
object AvSTests extends App{

  
  val tolerance = 0.001
  
  val testK = andPr( liftPr((d:Double)=>d==100, takePr(10, bigK[Double](100))))
 
  val testProb = probabilityLattice(10).sum - 1 < tolerance
  
  val xm = ExampleModel.makeModel(mkDate(0))
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
 
  val testSuite = List( testK, testProb, testPr1)
  assert( testSuite.forall(identity), "Test suite falure: " + testSuite )
}