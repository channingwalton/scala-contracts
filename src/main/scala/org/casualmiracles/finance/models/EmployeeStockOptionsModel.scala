package org.casualmiracles.finance.models

/**
 * @author nz
 */
import org.casualmiracles.finance.contracts._

object EmployeeStockOptionsModel extends GenericModel with GenericComputations with GeometricInterestRateModel {
  override def makeModel(modelDate: Date, inputsData:ModelParameters) = Model(
    modelStart = modelDate,
    disc = (discount( crr, continuouscomp, inputsData.p _, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    snell = (discount( snell, continuouscomp, inputsData.p _, _:Currency, _:PR[Boolean], _:PR[Double])).curried,
    exch = (exch _).curried,
    comp= (comp _).curried,
    absorb = (absorb _).curried,
    rateModel = rateModel _,    
    inputs = inputsData)  
}

class EmployeeStockOptionsModel extends GenericModel with GeometricInterestRateModel{
  
}