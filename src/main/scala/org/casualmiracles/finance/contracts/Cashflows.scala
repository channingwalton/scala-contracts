package org.casualmiracles.finance.contracts

import Contracts._

object Cashflows {

  def cashflow(contract: Contract): RV[Double] = contract match {
    case Zero => Stream(0)
    case _ => sys.error("not implemented")
  }
}