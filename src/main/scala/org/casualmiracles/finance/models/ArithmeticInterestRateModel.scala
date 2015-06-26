package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._
import Contracts._
import org.casualmiracles.finance.models._
import Stream._

trait ArithmeticInterestRateModel extends InterestRateModel {
  // Arithmetic Interest Rate Model
  def rates(rateNow: Double, delta: Double): PR[Double] = {
    def makeRateSlices(rateNow: Double, n: Int): Stream[RV[Double]] = rateSlice(rateNow, n) #:: makeRateSlices(rateNow - delta, n + 1)
    def rateSlice(minRate: Double, n: Int) = comb(minRate).take(n)
    def comb(x: Double): Stream[Double] = x #:: comb(x + 2 * delta)
    PR(makeRateSlices(rateNow, 1))
  }
  import scala.collection.mutable.Map
  override val rateModels: Map[Currency, PR[Double]] = Map(
    CHF -> rates(7, 0.8),
    EUR -> rates(6.5, 0.25),
    GBP -> rates(8, 0.5),
    KYD -> rates(11, 1.2),
    USD -> rates(5, 1),
    ZAR -> rates(15, 1.5))
}