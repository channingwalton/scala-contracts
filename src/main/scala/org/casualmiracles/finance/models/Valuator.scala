package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts.Contract
import org.casualmiracles.finance.contracts.Contracts.mkDate
import org.casualmiracles.finance.contracts.Date
import org.casualmiracles.finance.contracts.Instruments.americancall
import org.casualmiracles.finance.contracts.Instruments.stock
import org.casualmiracles.finance.contracts.USD

/**
 * @author yl
 */

object Valuator {
  //
  // Valuation execution wth formally defined interface
  //
  private def getModel(model:String, date:Date, params:ModelParameters):Model = model match {
    case "eber" => EberModel.makeModel(date, params)
    case "eso" => EmployeeStockOptionsModel.makeModel(date, params)
    case "fe1" => LatticeFE1Model.makeModel(date, params)
  } 
  private def getEngine(model:String):GenericModel = model match {
    case "eso" => new EmployeeStockOptionsModel()
  }
  
  // TODO: Model Date; 
  // TODO: Model Currency;
  def valuateContract( contractParams: ContractParameters): ContractValuation =
    contractParams.contract match {
    case "americancallonstock" => {
      
      val modelParams = new ModelParameters(r = contractParams.interest, 
          sigma = contractParams.volatility,
          div = contractParams.divident, step=contractParams.expiry/contractParams.steps)
      
      val model = getModel(contractParams.model, mkDate(0), modelParams)
      
      val engine = getEngine(contractParams.model)
      
      engine.tracer.tracing = contractParams.trace 
      if( contractParams.trace ){        
        engine.tracer.mode = "acc"
      }
      
      val evaluator = engine.evalC(model, USD)
      
      val underlying: Contract = stock(contractParams.asset)
      
      val contract = americancall(mkDate(contractParams.steps), underlying, contractParams.strike, USD ) 
      
      val pr = evaluator(contract)
      
      val trace:Option[String] = if(contractParams.trace) Some(engine.tracer.output()) else None
      
      ContractValuation(pr=Some(pr), trace=trace)
    }
    case _ => ContractValuation(errors = Some("Contract Type: is Not Defined." :: Nil))
    }
}