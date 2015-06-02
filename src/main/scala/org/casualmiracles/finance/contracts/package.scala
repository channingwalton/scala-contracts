package org.casualmiracles.finance

package object contracts {
  type RV[A] = Stream[A]
  type TimeStep = Int
  type CalendarTime = Unit
}