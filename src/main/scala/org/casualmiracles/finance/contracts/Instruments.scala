package org.casualmiracles.finance.contracts

object Instruments {

  import Contracts._

  def zeroCouponBond(d: Date, n: Double, c: Currency): Contract = when(at(d))(scale(n)(One(c)))
  def european(d: Date, c: Contract): Contract = when(at(d))(c or Zero)
  def american(d1: Date, d2: Date, c: Contract): Contract = anytime(between(d1, d2))(c)
  def swap(give: Contract, take: Contract) = take andGive give
}