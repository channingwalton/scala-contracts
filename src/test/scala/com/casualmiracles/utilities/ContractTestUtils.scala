package org.casualmiracles.utilities

import org.casualmiracles.finance.contracts.PR
import PR._
import org.casualmiracles.finance.models.GenericComputations

/**
 * @author nz
 */
trait ContractTestUtils extends GenericComputations{
  val tolerance = 0.01
  
  def compare2Pr(a:PR[Double], b:PR[Double],tolerate:Double=tolerance):Boolean =
     andPr(lift2Pr((a:Double, b:Double) => Math.abs(a-b)<tolerate,a,b))
}