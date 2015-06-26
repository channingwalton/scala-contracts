package org.casualmiracles.finance.examples

import org.casualmiracles.finance.contracts._
import Contracts._
import Instruments._
import org.casualmiracles.finance.models._
import EberModel._
import Cashflows._

// TODO: Convert to EberModelSuite.scala
object Examples extends App {

  val mps = new ModelParameters().p(0.5)

  //mps.p = 
  val xm: Model = EberModel.makeModel(mkDate(0), mps)
  val evalX: Contract ⇒ PR[Double] = EberModel.evalC(xm, USD)
  val t1Horizon = 3
  val t1 = mkDate(t1Horizon)
  val c1: Contract = zeroCouponBond(t1, 10, USD)

  val c11 = european(mkDate(20),
    zeroCouponBond(mkDate(20), 0.4, USD) and
      zeroCouponBond(mkDate(30), 9.3, USD) and
      zeroCouponBond(mkDate(40), 109.3, USD) andGive zeroCouponBond(mkDate(12), 100.0, GBP))

  println("C1")
  
  println( formatPr(evalX(c1), 10) )

  def absorbEx(t: Date, x: Double, k: Currency) = until(constant(t) %> date)(scale(x)(one(k)))

  // some examples from the paper
  val t2 = mkDate(10)

  def rainInCyprus = constant(10.0) // something that generates rainfall figures
  def interestRate = constant(1.0) // obviously need a real source of interest rates

  val c8 = scale(rainInCyprus)(One(GBP))

  val c9 = scale((rainInCyprus %- 7) %* 1000)(One(GBP))

  val c10 = cond(rainInCyprus %> 9)(c8)(c9)

  val c12 = until(interestRate %> 6)(american(t1, t2, c10))

  println("c1 cashflow")
  println( formatPr(cashflow(xm, USD, 10)(c11), 100) )
}