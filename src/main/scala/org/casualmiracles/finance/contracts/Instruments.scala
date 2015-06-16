package org.casualmiracles.finance.contracts

object Instruments {

  import Contracts._

  def buy(c:Contract, n:Double, curr:Currency): Contract = c andGive( scale(n)(One(curr)) ) 
  def sell(c:Contract, n:Double, curr:Currency): Contract = scale(n)(One(curr)) andGive(c)
  
  def zeroCouponBond(d: Date, n: Double, c: Currency): Contract = when(at(d))(scale(n)(One(c)))
  
  // TODO: stock is a modelled as a compounded-price security
  def stock(d: Date, n: Double, c: Currency): Contract = upto(at(d))(scale(n)(One(c)))
 
  def option(c:Contract): Contract = c or Zero
  def europeancall(d: Date, c: Contract, strike:Double, curr:Currency): Contract = when(at(d))(option(buy(c,strike,curr)))
  def americancall(d: Date, c: Contract, strike:Double, curr:Currency): Contract = anytime(at(d))(option(buy(c,strike,curr)))
  // TODO: europeanput
  // TODO: change at() to between()
  def americanput(d: Date, c: Contract, strike:Double, curr:Currency): Contract = anytime(at(d))(option(sell(c,strike,curr)))
  
  // TODO: remove and refactor usage in Examples.scala 
  def european(d: Date, c: Contract): Contract = when(at(d))(c or Zero)
  def american(d1: Date, d2: Date, c: Contract): Contract = anytime(between(d1, d2))(c)
  def swap(give: Contract, take: Contract) = take andGive give
}