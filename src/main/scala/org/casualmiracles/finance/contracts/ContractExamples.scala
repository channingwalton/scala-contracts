package org.casualmiracles.finance.contracts
import Contracts._

object ContractExamples extends App {

  val xm = exampleModel(mkDate(0))
  val evalX = evalC(xm, USD)
  val t1Horizon = 3
  val t1 = mkDate(t1Horizon)
  val c1 = zcb(t1, 10, USD)
  
  
   val c11 = european(mkDate(2),
    zcb(mkDate(20), 0.4, USD) and
    zcb(mkDate(30), 9.3, USD) and
    zcb(mkDate(40), 109.3, USD) andGive (zcb(mkDate(12), 100.0, GBP)))
  
  val pr1 = evalX(c1)
  printPr(pr1,10)
  
  def absorbEx(t: Date, x:Double, k: Currency) = until (konst(t) %> date) (scale (konst(x)) (one(k)))
  
}