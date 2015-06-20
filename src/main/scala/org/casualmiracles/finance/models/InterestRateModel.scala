package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._

trait InterestRateModel {
  
  def rateModels: Map[Currency, PR[Double]] = Map()
  
  def rateModel(k: Currency): PR[Double] = rateModels.getOrElse(k, sys.error("rateModel: currency not found " + k))
}