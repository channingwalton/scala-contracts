package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._
import Contracts._
import Instruments._
import Stream._

trait GeometricInterestRateModel extends InterestRateModel {

  // Geometric Interest Rate Model
  def ratesUpDown(rateNow: Double, up: Double, down: Double): PR[Double] = {
    def makeRateSlices(rateNow: Double, n: Int): Stream[RV[Double]] = rateSlice(rateNow, n) #:: makeRateSlices( rateNow * down, n + 1)
    def rateSlice(minRate: Double, n: Int) = comb(minRate).take(n)
    def comb(x:Double): Stream[Double] = x #:: comb(x/down*up)
    PR(makeRateSlices(rateNow, 1))
  }
  import scala.collection.mutable.Map
  override val rateModels: Map[Currency, PR[Double]] = Map(
    USD -> ratesUpDown(5, 1, 1)
    //USD -> ratesUpDown(6, 1.25, 0.9)    
  )  
  
}