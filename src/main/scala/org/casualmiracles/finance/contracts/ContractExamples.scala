package org.casualmiracles.finance.contracts
import Contracts._
import ExampleModel._

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
  
  println("C1")
  printPr(evalX(c1),10)

  println("\nRate evolution")
  printPr(takePr(t1Horizon + 1, xm.rateModel(USD)), t1Horizon + 1)
  
  def absorbEx(t: Date, x:Double, k: Currency) = until (konst(t) %> date) (scale (konst(x)) (one(k)))
  
  
  // some examples from the paper
  val t2 = mkDate(10)
  
  def rainInCyprus = konst(10.0) // something that generates rainfall figures
  def interestRate = konst(1.0) // obviously need a real source of interest rates

  val c8 = scale(rainInCyprus)(One(GBP))

  val c9 = scale((rainInCyprus - 7) * 1000)(One(GBP))

  val c10 = cond(rainInCyprus %> 10)(c8)(c9)

  val c12 = until(interestRate %> 6)(american(t1, t2, c10))
  
}