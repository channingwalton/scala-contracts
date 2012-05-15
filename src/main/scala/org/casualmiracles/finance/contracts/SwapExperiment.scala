package org.casualmiracles.finance.contracts

import Contracts._
import ExampleModel._
import Cashflows._
import Instruments._

object SwapExperiment extends App {

  def interestRate = konst(1.0) // obviously need a real source of interest rates

  def swap(give: Contract, take: Contract) = take andGive give

  def fixedRate(notional: Double, currency: Currency, rate: Double) = scale(notional * rate)(One(currency))

  def floatingRate(notional: Double, currency: Currency, float: Observable[Double]) = scale(float * notional)(One(currency))

  def uniformSchedule(start: Date, end: Date, frequency: Int): Observable[Boolean] =
    between(start, end) %&& lift((d: Date) ⇒ (d.t - start.t) % frequency == 0, date)

 val example = when(uniformSchedule(mkDate(4), mkDate(8), 2))(swap(fixedRate(1, USD, 0.05), floatingRate(1, USD, interestRate)))
 
 val xm = exampleModel(mkDate(0))
 val evalX = evalC(xm, USD)
 
 printPr(cashflow(xm, USD)(example), 10)
}