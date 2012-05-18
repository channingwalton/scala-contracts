package org.casualmiracles.finance.contracts

case class Date(c: CalendarTime, t: TimeStep) extends Ordered[Date] {
  def compare(that: Date) = t.compare(that.t)
}