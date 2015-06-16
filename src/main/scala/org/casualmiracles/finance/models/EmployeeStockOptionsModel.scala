package org.casualmiracles.finance.models

/**
 * @author nz
 */
import org.casualmiracles.finance.contracts._

object EmployeeStockOptionsModel extends GenericModel with GeometricInterestRateModel {
  override def makeModel(modelDate: Date, p:Double) = Model(
    modelStart = modelDate,
    disc = (discount( crr, continuouscomp, p, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    snell = (discount( snell, continuouscomp, p, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    exch = (exch _).curried,
    absorb = (absorb _).curried,
    rateModel = rateModel _,
    
    sigma = 0.25)  
}