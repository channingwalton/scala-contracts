package org.casualmiracles.finance.examples

import org.casualmiracles.finance.contracts._
import Contracts._
import org.casualmiracles.finance.models._
import ExampleModel._
import Cashflows._
import Instruments._
import org.casualmiracles.finance.models.Cashflows

object SwapExperiment extends App {

  def interestRate = constant(1.0) // obviously need a real source of interest rates

  def fixedRate(notional: Double, currency: Currency, rate: Double) = scale(notional * rate)(One(currency))

  def floatingRate(notional: Double, currency: Currency, rate: Observable[Double]) = scale(rate %* notional)(One(currency))

  def uniformSchedule(start: Date, end: Date, frequency: Int): Observable[Boolean] = between(start, end) %&& ((date %- start) %% frequency %== 0)

 val fixedSchedule = uniformSchedule(mkDate(4), mkDate(10), 2)
 val floatingSchedule = uniformSchedule(mkDate(4), mkDate(10), 3)
 
 val fixedLeg = when(fixedSchedule)(fixedRate(1, USD, 0.05))
 val floatingLeg = when(floatingSchedule)(floatingRate(1, USD, interestRate))
 
 val example = swap(fixedLeg, floatingLeg)
 
 val horizon = 15
 val xm = ExampleModel.makeModel(time0)
 val evalX = ExampleModel.evalC(xm, USD)
 
 printPr(cashflow(xm, USD, horizon)(example), horizon)
}